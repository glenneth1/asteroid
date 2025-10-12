#!/bin/bash
# test-server.sh - Comprehensive test suite for Asteroid Radio server
# Tests all API endpoints and core functionality

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
BASE_URL="${ASTEROID_URL:-http://localhost:8080}"
API_BASE="${BASE_URL}/api/asteroid"
VERBOSE="${VERBOSE:-0}"
LOG_FILE="test-results-$(date +%Y%m%d-%H%M%S).log"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Helper functions
log_to_file() {
    # Strip ANSI color codes for log file
    echo "$1" | sed 's/\x1b\[[0-9;]*m//g' >> "$LOG_FILE"
}

print_header() {
    local msg="\n========================================\n$1\n========================================\n"
    echo -e "\n${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}\n"
    log_to_file "$msg"
}

print_test() {
    echo -e "${YELLOW}TEST:${NC} $1"
    log_to_file "TEST: $1"
}

print_pass() {
    echo -e "${GREEN}✓ PASS:${NC} $1"
    log_to_file "✓ PASS: $1"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

print_fail() {
    echo -e "${RED}✗ FAIL:${NC} $1"
    log_to_file "✗ FAIL: $1"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

print_info() {
    echo -e "${BLUE}INFO:${NC} $1"
    log_to_file "INFO: $1"
}

# Test function wrapper
run_test() {
    local test_name="$1"
    TESTS_RUN=$((TESTS_RUN + 1))
    print_test "$test_name"
}

# Check if server is running
check_server() {
    print_header "Checking Server Status"
    run_test "Server is accessible"
    
    if curl -s --max-time 5 "${BASE_URL}/asteroid/" > /dev/null 2>&1; then
        print_pass "Server is running at ${BASE_URL}"
    else
        print_fail "Server is not accessible at ${BASE_URL}"
        echo "Please start the server with: ./asteroid"
        exit 1
    fi
}

# Test API endpoint with JSON response
test_api_endpoint() {
    local endpoint="$1"
    local description="$2"
    local expected_field="$3"
    local method="${4:-GET}"
    local data="${5:-}"
    
    run_test "$description"
    
    local url="${API_BASE}${endpoint}"
    local response
    
    if [ "$method" = "POST" ]; then
        response=$(curl -s -X POST "$url" ${data:+-d "$data"})
    else
        response=$(curl -s "$url")
    fi
    
    if [ $VERBOSE -eq 1 ]; then
        echo "Response: $response" | head -c 200
        echo "..."
    fi
    
    # Check if response contains expected field
    if echo "$response" | grep -q "$expected_field"; then
        print_pass "$description - Response contains '$expected_field'"
        return 0
    else
        print_fail "$description - Expected field '$expected_field' not found"
        if [ $VERBOSE -eq 1 ]; then
            echo "Full response: $response"
        fi
        return 1
    fi
}

# Test JSON structure
test_json_structure() {
    local endpoint="$1"
    local description="$2"
    local jq_query="$3"
    
    run_test "$description"
    
    local url="${API_BASE}${endpoint}"
    local response=$(curl -s "$url")
    
    # Check if jq is available
    if ! command -v jq &> /dev/null; then
        print_info "jq not installed, skipping JSON validation"
        return 0
    fi
    
    if echo "$response" | jq -e "$jq_query" > /dev/null 2>&1; then
        print_pass "$description"
        return 0
    else
        print_fail "$description"
        if [ $VERBOSE -eq 1 ]; then
            echo "Response: $response"
        fi
        return 1
    fi
}

# Test Status Endpoints
test_status_endpoints() {
    print_header "Testing Status Endpoints"
    
    test_api_endpoint "/status" \
        "Server status endpoint" \
        "asteroid-radio"
    
    test_api_endpoint "/auth-status" \
        "Authentication status endpoint" \
        "loggedIn"
    
    test_api_endpoint "/icecast-status" \
        "Icecast status endpoint" \
        "icestats"
}

# Test Admin Endpoints (requires authentication)
test_admin_endpoints() {
    print_header "Testing Admin Endpoints"
    
    print_info "Note: Admin endpoints require authentication"
    
    test_api_endpoint "/admin/tracks" \
        "Admin tracks listing" \
        "data"
    
    # Note: scan-library is POST and modifies state, so we just check it exists
    run_test "Admin scan-library endpoint exists"
    local response=$(curl -s -X POST "${API_BASE}/admin/scan-library")
    if echo "$response" | grep -q "status"; then
        print_pass "Admin scan-library endpoint responds"
    else
        print_fail "Admin scan-library endpoint not responding"
    fi
}

# Test Track Endpoints
test_track_endpoints() {
    print_header "Testing Track Endpoints"
    
    test_api_endpoint "/tracks" \
        "Tracks listing endpoint" \
        "data"
}

# Test Player Endpoints
test_player_endpoints() {
    print_header "Testing Player Control Endpoints"
    
    test_api_endpoint "/player/status" \
        "Player status endpoint" \
        "player"
    
    test_api_endpoint "/player/pause" \
        "Player pause endpoint" \
        "status"
    
    test_api_endpoint "/player/stop" \
        "Player stop endpoint" \
        "status"
    
    test_api_endpoint "/player/resume" \
        "Player resume endpoint" \
        "status"
}

# Test Playlist Endpoints
test_playlist_endpoints() {
    print_header "Testing Playlist Endpoints"
    
    test_api_endpoint "/playlists" \
        "Playlists listing endpoint" \
        "data"
    
    # Test playlist creation (requires auth)
    print_info "Note: Playlist creation requires authentication"
}

# Test DJ Control Endpoints (Liquidsoap telnet integration)
test_dj_control_endpoints() {
    print_header "Testing DJ Control Endpoints"
    
    print_info "Note: DJ control endpoints require admin authentication"
    
    # Test metadata endpoint (available to authenticated users)
    test_api_endpoint "/dj/metadata" \
        "DJ metadata endpoint" \
        "metadata"
    
    # Test queue status endpoint
    test_api_endpoint "/dj/queue-status" \
        "DJ queue status endpoint" \
        "queue"
    
    # Test skip track endpoint (requires admin)
    run_test "DJ skip track endpoint"
    local skip_response=$(curl -s -X POST "${API_BASE}/dj/skip")
    if echo "$skip_response" | grep -q "status"; then
        print_pass "DJ skip track endpoint responds"
    else
        print_fail "DJ skip track endpoint not responding"
    fi
    
    # Test reload playlist endpoint (requires admin)
    run_test "DJ reload playlist endpoint"
    local reload_response=$(curl -s -X POST "${API_BASE}/dj/reload-playlist")
    if echo "$reload_response" | grep -q "status"; then
        print_pass "DJ reload playlist endpoint responds"
    else
        print_fail "DJ reload playlist endpoint not responding"
    fi
    
    # Test queue track endpoint with dynamic track ID
    run_test "DJ queue track endpoint"
    
    # Fetch a random track ID from the database
    local track_response=$(curl -s "${API_BASE}/tracks?page=1&per-page=1")
    
    # Extract track ID using grep (works without jq)
    local track_id=$(echo "$track_response" | grep -o '"id":[0-9]*' | head -1 | grep -o '[0-9]*')
    
    if [ -n "$track_id" ]; then
        print_info "Using track ID: $track_id for queue test"
        local queue_response=$(curl -s -X POST "${API_BASE}/dj/queue?track-id=${track_id}")
        
        if echo "$queue_response" | grep -q "queued successfully\|Track not found"; then
            print_pass "DJ queue track endpoint responds (track ID: $track_id)"
        else
            print_fail "DJ queue track endpoint not responding properly"
            if [ $VERBOSE -eq 1 ]; then
                echo "Response: $queue_response"
            fi
        fi
    else
        print_info "No tracks available in database, skipping queue test"
    fi
}

# Test Page Endpoints (HTML pages)
test_page_endpoints() {
    print_header "Testing HTML Page Endpoints"
    
    run_test "Front page loads"
    if curl -s "${BASE_URL}/asteroid/" | grep -q "ASTEROID RADIO"; then
        print_pass "Front page loads successfully"
    else
        print_fail "Front page not loading"
    fi
    
    run_test "Admin page loads"
    if curl -s "${BASE_URL}/asteroid/admin" | grep -q "ADMIN DASHBOARD"; then
        print_pass "Admin page loads successfully"
    else
        print_fail "Admin page not loading"
    fi
    
    run_test "Player page loads"
    if curl -s "${BASE_URL}/asteroid/player" | grep -q "Web Player"; then
        print_pass "Player page loads successfully"
    else
        print_fail "Player page not loading"
    fi
}

# Test Static File Serving
test_static_files() {
    print_header "Testing Static File Serving"
    
    run_test "CSS file loads"
    if curl -s -I "${BASE_URL}/asteroid/static/asteroid.css" | grep -q "200 OK"; then
        print_pass "CSS file accessible"
    else
        print_fail "CSS file not accessible"
    fi
    
    run_test "JavaScript files load"
    if curl -s -I "${BASE_URL}/asteroid/static/js/player.js" | grep -q "200 OK"; then
        print_pass "JavaScript files accessible"
    else
        print_fail "JavaScript files not accessible"
    fi
}

# Test API Response Format
test_api_format() {
    print_header "Testing API Response Format"
    
    run_test "API returns JSON format"
    local response=$(curl -s "${API_BASE}/status")
    
    if echo "$response" | grep -q '"status"'; then
        print_pass "API returns JSON (not S-expressions)"
    else
        print_fail "API not returning proper JSON format"
        if [ $VERBOSE -eq 1 ]; then
            echo "Response: $response"
        fi
    fi
}

# Print summary
print_summary() {
    print_header "Test Summary"
    
    local summary="Tests Run:    $TESTS_RUN\nTests Passed: $TESTS_PASSED\nTests Failed: $TESTS_FAILED"
    
    echo "Tests Run:    $TESTS_RUN"
    echo -e "Tests Passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests Failed: ${RED}$TESTS_FAILED${NC}"
    log_to_file "$summary"
    
    if [ $TESTS_FAILED -eq 0 ]; then
        echo -e "\n${GREEN}✓ All tests passed!${NC}\n"
        log_to_file "\n✓ All tests passed!\n"
        echo -e "${BLUE}Log file saved to: ${LOG_FILE}${NC}\n"
        exit 0
    else
        echo -e "\n${RED}✗ Some tests failed${NC}\n"
        log_to_file "\n✗ Some tests failed\n"
        echo -e "${BLUE}Log file saved to: ${LOG_FILE}${NC}\n"
        exit 1
    fi
}

# Main test execution
main() {
    # Initialize log file with header
    cat > "$LOG_FILE" << EOF
╔═══════════════════════════════════════════════════════════════╗
║           Asteroid Radio Server Test Suite                    ║
║                    Test Results Log                           ║
╚═══════════════════════════════════════════════════════════════╝

Test Run Date: $(date '+%Y-%m-%d %H:%M:%S')
Server URL: ${BASE_URL}
API Base: ${API_BASE}
Verbose Mode: ${VERBOSE}

EOF
    
    echo -e "${BLUE}"
    echo "╔═══════════════════════════════════════╗"
    echo "║  Asteroid Radio Server Test Suite    ║"
    echo "╔═══════════════════════════════════════╗"
    echo -e "${NC}"
    
    print_info "Testing server at: ${BASE_URL}"
    print_info "Verbose mode: ${VERBOSE}"
    print_info "Log file: ${LOG_FILE}"
    echo ""
    
    # Run all test suites
    check_server
    test_api_format
    test_status_endpoints
    test_track_endpoints
    test_player_endpoints
    test_playlist_endpoints
    test_dj_control_endpoints
    test_admin_endpoints
    test_page_endpoints
    test_static_files
    
    # Print summary
    print_summary
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        -u|--url)
            BASE_URL="$2"
            API_BASE="${BASE_URL}/api/asteroid"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  -v, --verbose    Enable verbose output"
            echo "  -u, --url URL    Set base URL (default: http://localhost:8080)"
            echo "  -h, --help       Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  ASTEROID_URL     Base URL for the server"
            echo "  VERBOSE          Enable verbose output (0 or 1)"
            echo ""
            echo "Examples:"
            echo "  $0                           # Test local server"
            echo "  $0 -v                        # Verbose mode"
            echo "  $0 -u http://example.com     # Test remote server"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

# Run main
main
