SCRIPT_DIR="$(cd $(dirname "$0"); pwd)"
TEST_FILE="${SCRIPT_DIR}/tests.js"

(
  cd "${SCRIPT_DIR}"
  elm-package install -y
  elm-make Tests.elm --yes --output "${TEST_FILE}"
  node "${TEST_FILE}"
  rm -f "${TEST_FILE}"
)