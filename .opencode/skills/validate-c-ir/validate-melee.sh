#!/usr/bin/env bash
# Validates the C headers + doldecomp symbols -> IR pipeline against the Melee
# decompilation reference corpus. See the SKILL.md next to this script for the
# full definition of done.
#
# Usage:
#   .opencode/skills/validate-c-ir/validate-melee.sh [--strict]
#
# --strict: treat desired-future-state checks (@http traits, structure-name
#           operation naming) as hard failures instead of advisory.
#
# Exit codes:
#   0  all hard checks passed (advisory failures are reported but do not fail)
#   1  at least one hard check failed
#   2  environment error (sbt missing, melee corpus missing, server did not start)

set -uo pipefail

STRICT=0
for arg in "$@"; do
  case "$arg" in
    --strict) STRICT=1 ;;
    -h|--help)
      sed -n '2,20p' "$0"; exit 0 ;;
    *) echo "unknown arg: $arg" >&2; exit 2 ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
SKILL_DIR="$REPO_ROOT/.opencode/skills/validate-c-ir"

MELEE_SYMBOLS="${MELEE_SYMBOLS:-$HOME/projects/ai/yolo/melee/config/GALE01/symbols.txt}"
MELEE_SRC="${MELEE_SRC:-$HOME/projects/ai/yolo/melee/src}"
SMITHY_OUT="$REPO_ROOT/generated/melee.smithy"

NAMESPACE="melee"
SERVICE="MasterHand"
WORD_SIZE=32

CHEADERS_PORT=${CHEADERS_PORT:-8080}
SMITHY_PORT=${SMITHY_PORT:-8081}

PASS_COUNT=0
FAIL_COUNT=0
ADVISORY_COUNT=0

green()  { printf '\033[32m%s\033[0m\n' "$*"; }
red()    { printf '\033[31m%s\033[0m\n' "$*"; }
yellow() { printf '\033[33m%s\033[0m\n' "$*"; }

record_pass()     { green "PASS  $1"; PASS_COUNT=$((PASS_COUNT + 1)); }
record_fail()     { red   "FAIL  $1"; FAIL_COUNT=$((FAIL_COUNT + 1)); }
record_advisory() { yellow "ADVISORY  $1  (not enforced; use --strict to fail)"; ADVISORY_COUNT=$((ADVISORY_COUNT + 1)); }

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || { red "missing required command: $1"; exit 2; }
}

require_cmd sbt
require_cmd curl
require_cmd jq
require_cmd grep

[ -f "$MELEE_SYMBOLS" ] || { red "melee symbols file not found: $MELEE_SYMBOLS"; exit 2; }
[ -d "$MELEE_SRC" ]     || { red "melee src dir not found: $MELEE_SRC"; exit 2; }

mkdir -p "$(dirname "$SMITHY_OUT")"

echo "==> generating Smithy model from melee C headers"
sbt -batch -error "run cheaders-smithy \
  --symbols $MELEE_SYMBOLS \
  --headers $MELEE_SRC \
  --word-size $WORD_SIZE \
  --namespace $NAMESPACE \
  --service $SERVICE \
  --output $SMITHY_OUT" > /tmp/melee-gen.log 2>&1
if [ $? -ne 0 ]; then
  red "sbt cheaders-smithy failed; tail of log:"
  tail -n 60 /tmp/melee-gen.log >&2
  exit 1
fi
record_pass "generated $SMITHY_OUT"

[ -s "$SMITHY_OUT" ] && record_pass "smithy output is non-empty" || record_fail "smithy output is empty"

# --- Static inspection of generated smithy ----------------------------------

echo "==> inspecting generated smithy"

if grep -qE "^service $SERVICE \{" "$SMITHY_OUT"; then
  record_pass "service $SERVICE declared"
else
  record_fail "service $SERVICE not declared"
fi

if grep -qE "@(com\.jacoby6000\.daphttp#)?wordSize\(\s*${WORD_SIZE}\s*\)" "$SMITHY_OUT"; then
  record_pass "@wordSize($WORD_SIZE) present on service"
else
  record_fail "@wordSize($WORD_SIZE) missing on service"
fi

operation_count=$(grep -cE "^operation " "$SMITHY_OUT" || true)
if [ "$operation_count" -gt 0 ]; then
  green "PASS  generated $operation_count operations (one per statically accessible structure)"
  PASS_COUNT=$((PASS_COUNT + 1))
else
  red "FAIL  no operations generated"
  FAIL_COUNT=$((FAIL_COUNT + 1))
fi

# Desired future state: each operation should carry a standard Smithy 2.0
# @http(method: "GET", uri: "/api/MasterHand/<StructureName>") trait.
http_trait_count=$(grep -cE '@http\(\s*method:\s*"GET"' "$SMITHY_OUT" || true)
if [ "$http_trait_count" -ge "$operation_count" ] && [ "$operation_count" -gt 0 ]; then
  record_pass "@http GET trait attached to every operation ($http_trait_count/$operation_count)"
else
  msg="@http GET trait missing on most operations ($http_trait_count/$operation_count)"
  if [ "$STRICT" -eq 1 ]; then record_fail "$msg"; else record_advisory "$msg"; fi
fi

# Desired future state: operation names should be the C structure names, not
# "Get<SymbolName>". Detect the legacy naming pattern.
get_prefix_count=$(grep -cE "^operation Get[A-Za-z]" "$SMITHY_OUT" || true)
if [ "$get_prefix_count" -eq 0 ]; then
  record_pass "operations are not using legacy Get<PascalCase> naming"
else
  msg="legacy Get<PascalCase> operation naming still present on $get_prefix_count operations"
  if [ "$STRICT" -eq 1 ]; then record_fail "$msg"; else record_advisory "$msg"; fi
fi

# GET-only: any non-GET @http methods would be a violation.
non_get_http_count=$(grep -cE '@http\(\s*method:\s*"(POST|PUT|PATCH|DELETE)"' "$SMITHY_OUT" || true)
if [ "$non_get_http_count" -eq 0 ]; then
  record_pass "no mutation @http methods present (GET-only)"
else
  record_fail "found $non_get_http_count non-GET @http methods (must be GET-only)"
fi

# --- HTTP smoke: load C headers directly and inspect /routes ---------------

echo "==> starting cheaders server on port $CHEADERS_PORT"

sbt -batch -error "run cheaders \
  --symbols $MELEE_SYMBOLS \
  --headers $MELEE_SRC \
  --word-size $WORD_SIZE \
  --namespace $NAMESPACE \
  --service $SERVICE \
  --bind-host 127.0.0.1 \
  --bind-port $CHEADERS_PORT" > /tmp/melee-cheaders.log 2>&1 &
CHEADERS_PID=$!

cleanup() {
  if [ -n "${CHEADERS_PID:-}" ] && kill -0 "$CHEADERS_PID" 2>/dev/null; then
    kill "$CHEADERS_PID" 2>/dev/null || true
    wait "$CHEADERS_PID" 2>/dev/null || true
  fi
  if [ -n "${SMITHY_PID:-}" ] && kill -0 "$SMITHY_PID" 2>/dev/null; then
    kill "$SMITHY_PID" 2>/dev/null || true
    wait "$SMITHY_PID" 2>/dev/null || true
  fi
}
trap cleanup EXIT

wait_for_http() {
  local port="$1" tries=120
  while [ "$tries" -gt 0 ]; do
    if curl -sf "http://127.0.0.1:${port}/health" >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
    tries=$((tries - 1))
  done
  return 1
}

if wait_for_http "$CHEADERS_PORT"; then
  record_pass "cheaders /health responding"
else
  red "FAIL  cheaders server did not start; tail of log:"
  tail -n 60 /tmp/melee-cheaders.log >&2
  exit 1
fi

cheaders_routes_json=$(curl -sf "http://127.0.0.1:${CHEADERS_PORT}/routes" || true)
if [ -z "$cheaders_routes_json" ]; then
  record_fail "cheaders /routes returned no body"
else
  record_pass "cheaders /routes responds"
  cheaders_route_count=$(printf '%s' "$cheaders_routes_json" | jq '[.routes[]] | length')
  if [ "$cheaders_route_count" -gt 0 ]; then
    green "PASS  cheaders /routes lists $cheaders_route_count routes"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    red "FAIL  cheaders /routes is empty (expected one route per static structure)"
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi
  cheaders_errors=$(printf '%s' "$cheaders_routes_json" | jq '.errors | length')
  if [ "$cheaders_errors" -eq 0 ]; then
    record_pass "cheaders /routes reports no errors"
  else
    yellow "ADVISORY  cheaders /routes reports $cheaders_errors errors (partial symbols)"
    ADVISORY_COUNT=$((ADVISORY_COUNT + 1))
  fi
fi

# Smoke one data route: it should return HTTP 200 even without a DAP connection;
# the response will carry per-read error fields but the route itself resolves.
sample_route=$(printf '%s' "$cheaders_routes_json" | jq -r '.routes[0]' 2>/dev/null || true)
if [ -n "$sample_route" ]; then
  status=$(curl -s -o /tmp/melee-sample.json -w '%{http_code}' "http://127.0.0.1:${CHEADERS_PORT}${sample_route}" || true)
  if [ "$status" = "200" ]; then
    record_pass "GET $sample_route -> 200 (route resolves)"
  else
    record_fail "GET $sample_route -> $status (expected 200)"
  fi
else
  record_advisory "no sample route available to smoke (route list empty)"
fi

# --- Round-trip via the Smithy pipeline (the reference path) ----------------

echo "==> starting smithy server on port $SMITHY_PORT against $SMITHY_OUT"

sbt -batch -error "run smithy \
  --smithy $SMITHY_OUT \
  --bind-host 127.0.0.1 \
  --bind-port $SMITHY_PORT" > /tmp/melee-smithy.log 2>&1 &
SMITHY_PID=$!

if wait_for_http "$SMITHY_PORT"; then
  record_pass "smithy /health responding"
else
  red "FAIL  smithy server did not start; tail of log:"
  tail -n 60 /tmp/melee-smithy.log >&2
  exit 1
fi

smithy_routes_json=$(curl -sf "http://127.0.0.1:${SMITHY_PORT}/routes" || true)
if [ -z "$smithy_routes_json" ]; then
  record_fail "smithy /routes returned no body"
else
  record_pass "smithy /routes responds"
  smithy_route_count=$(printf '%s' "$smithy_routes_json" | jq '[.routes[]] | length')
  if [ "$smithy_route_count" -eq "$cheaders_route_count" ] && [ "$cheaders_route_count" -gt 0 ]; then
    green "PASS  smithy and cheaders expose the same route count ($smithy_route_count)"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    red "FAIL  smithy=$smithy_route_count cheaders=$cheaders_route_count route counts differ (C->IR != Smithy->IR)"
    FAIL_COUNT=$((FAIL_COUNT + 1))
  fi

  # Set difference of sorted route names. Order is irrelevant; identities must match.
  cheaders_set=$(printf '%s' "$cheaders_routes_json" | jq -r '.routes[]' | sort)
  smithy_set=$(printf '%s' "$smithy_routes_json"   | jq -r '.routes[]' | sort)
  diff_out=$(diff <(printf '%s' "$cheaders_set") <(printf '%s' "$smithy_set") || true)
  if [ -z "$diff_out" ]; then
    record_pass "smithy and cheaders expose the same route set"
  else
    record_fail "route set differs between cheaders and smithy:"
    printf '%s\n' "$diff_out" | sed 's/^/    /' >&2
  fi
fi

echo
echo "==> summary"
green "PASS:        $PASS_COUNT"
if [ "$ADVISORY_COUNT" -gt 0 ]; then yellow "ADVISORY:    $ADVISORY_COUNT"; fi
red   "FAIL:        $FAIL_COUNT"

if [ "$FAIL_COUNT" -gt 0 ]; then exit 1; fi
if [ "$STRICT" -eq 1 ] && [ "$ADVISORY_COUNT" -gt 0 ]; then
  red "FAIL: --strict set and $ADVISORY_COUNT advisory check(s) unmet"
  exit 1
fi
exit 0
