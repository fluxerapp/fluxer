/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package main

import (
	"bytes"
	"context"
	"crypto/rand"
	"embed"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"net/http/httputil"
	"net/url"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"golang.org/x/time/rate"
)

//go:embed assets
var assetsFS embed.FS

var metricsHost = os.Getenv("FLUXER_METRICS_HOST")
var metricsClient = &http.Client{
	Timeout: 5 * time.Second,
}

var defaultSentryProxyPath = "/error-reporting-proxy"
var sentryProxyPath = normalizeProxyPath(os.Getenv("SENTRY_PROXY_PATH"))
var sentryReportHost = func() string {
	if host := strings.TrimSpace(os.Getenv("SENTRY_REPORT_HOST")); host != "" {
		return strings.TrimRight(host, "/")
	}
	return ""
}()

func normalizeProxyPath(value string) string {
	clean := strings.TrimSpace(value)
	if clean == "" {
		return defaultSentryProxyPath
	}
	if !strings.HasPrefix(clean, "/") {
		clean = "/" + clean
	}
	if clean != "/" {
		clean = strings.TrimRight(clean, "/")
		if clean == "" {
			return "/"
		}
	}
	return clean
}

var staticCDNEndpoint = func() string {
	if v := strings.TrimSpace(os.Getenv("FLUXER_STATIC_CDN_ENDPOINT")); v != "" {
		return v
	}
	return "https://fluxerstatic.com"
}()

type responseWriter struct {
	http.ResponseWriter
	status      int
	size        int64
	wroteHeader bool
}

func (rw *responseWriter) WriteHeader(status int) {
	if !rw.wroteHeader {
		rw.status = status
		rw.wroteHeader = true
		rw.ResponseWriter.WriteHeader(status)
	}
}

func (rw *responseWriter) Write(b []byte) (int, error) {
	if !rw.wroteHeader {
		rw.WriteHeader(http.StatusOK)
	}
	size, err := rw.ResponseWriter.Write(b)
	rw.size += int64(size)
	return size, err
}

type ipLimiterEntry struct {
	limiter  *rate.Limiter
	lastSeen time.Time
}

type ipRateLimiter struct {
	mu       sync.Mutex
	limit    rate.Limit
	burst    int
	expiry   time.Duration
	limiters map[string]*ipLimiterEntry
}

const ipRateLimiterMaxEntries = 2048

func newIPRateLimiter(limit rate.Limit, burst int, expiry time.Duration) *ipRateLimiter {
	return &ipRateLimiter{
		limit:    limit,
		burst:    burst,
		expiry:   expiry,
		limiters: make(map[string]*ipLimiterEntry),
	}
}

func (r *ipRateLimiter) Allow(key string) bool {
	if key == "" {
		key = "unknown"
	}

	r.mu.Lock()
	defer r.mu.Unlock()

	entry := r.limiters[key]
	if entry == nil {
		entry = &ipLimiterEntry{
			limiter: rate.NewLimiter(r.limit, r.burst),
		}
		r.limiters[key] = entry
	}

	entry.lastSeen = time.Now()
	allowed := entry.limiter.Allow()

	if len(r.limiters) > ipRateLimiterMaxEntries {
		r.cleanup()
	}

	return allowed
}

func (r *ipRateLimiter) cleanup() {
	cutoff := time.Now().Add(-r.expiry)
	for key, entry := range r.limiters {
		if entry.lastSeen.Before(cutoff) {
			delete(r.limiters, key)
		}
	}
}

type Server struct {
	accessLog  *log.Logger
	errorLog   *log.Logger
	httpServer *http.Server

	assetsProxy     *httputil.ReverseProxy
	sentryProxy     *httputil.ReverseProxy
	sentryLimiter   *ipRateLimiter
	sentryProjectID string
	sentryPublicKey string
}

func NewServer() *Server {
	s := &Server{
		accessLog: log.New(os.Stdout, "[ACCESS] ", log.LstdFlags|log.LUTC),
		errorLog:  log.New(os.Stderr, "[ERROR] ", log.LstdFlags|log.LUTC|log.Lshortfile),
	}

	s.initAssetsProxy()
	s.initSentryProxy()
	return s
}

func (s *Server) initAssetsProxy() {
	target, err := url.Parse(staticCDNEndpoint)
	if err != nil {
		s.errorLog.Printf("Invalid FLUXER_STATIC_CDN_ENDPOINT %q: %v", staticCDNEndpoint, err)
		return
	}

	proxy := httputil.NewSingleHostReverseProxy(target)

	origDirector := proxy.Director
	proxy.Director = func(req *http.Request) {
		origDirector(req)
		req.Host = target.Host
		req.Header.Del("Cookie")
		req.Header.Del("Authorization")
	}

	proxy.ErrorHandler = func(w http.ResponseWriter, r *http.Request, err error) {
		s.errorLog.Printf("assets proxy error %s: %v", r.URL.Path, err)
		http.Error(w, "Bad Gateway", http.StatusBadGateway)
	}

	s.assetsProxy = proxy
}

func (s *Server) initSentryProxy() {
	rawDSN := strings.TrimSpace(os.Getenv("SENTRY_DSN"))
	if rawDSN == "" {
		return
	}

	parsed, err := url.Parse(rawDSN)
	if err != nil {
		s.errorLog.Printf("Invalid SENTRY_DSN %q: %v", rawDSN, err)
		return
	}

	if parsed.Scheme == "" || parsed.Host == "" {
		s.errorLog.Printf("Invalid SENTRY_DSN %q: missing scheme or host", rawDSN)
		return
	}

	pathPart := strings.Trim(parsed.Path, "/")
	var segments []string
	if pathPart != "" {
		segments = strings.Split(pathPart, "/")
	}

	if len(segments) == 0 {
		s.errorLog.Printf("Invalid SENTRY_DSN %q: missing project id", rawDSN)
		return
	}

	projectID := segments[len(segments)-1]
	prefixSegments := segments[:len(segments)-1]

	targetPathPrefix := ""
	if len(prefixSegments) > 0 {
		targetPathPrefix = "/" + strings.Join(prefixSegments, "/")
	}

	user := parsed.User
	if user == nil {
		s.errorLog.Printf("Invalid SENTRY_DSN %q: missing public key", rawDSN)
		return
	}

	publicKey := user.Username()
	if publicKey == "" {
		s.errorLog.Printf("Invalid SENTRY_DSN %q: missing public key", rawDSN)
		return
	}

	s.sentryPublicKey = publicKey
	s.sentryProjectID = projectID

	targetURL := &url.URL{
		Scheme: parsed.Scheme,
		Host:   parsed.Host,
	}

	proxy := httputil.NewSingleHostReverseProxy(targetURL)
	origDirector := proxy.Director
	proxy.Director = func(req *http.Request) {
		origDirector(req)
		trimmedPath := strings.TrimPrefix(req.URL.Path, sentryProxyPath)
		if trimmedPath == "" {
			trimmedPath = "/"
		} else if !strings.HasPrefix(trimmedPath, "/") {
			trimmedPath = "/" + trimmedPath
		}
		req.URL.Path = targetPathPrefix + trimmedPath
		req.URL.RawPath = req.URL.Path
		req.Host = targetURL.Host
		req.URL.Scheme = targetURL.Scheme
		req.URL.Host = targetURL.Host
		req.Header.Del("Cookie")
		req.Header.Del("Authorization")
	}

	proxy.ErrorHandler = func(w http.ResponseWriter, r *http.Request, err error) {
		s.errorLog.Printf("sentry proxy error %s: %v", r.URL.Path, err)
		http.Error(w, "Bad Gateway", http.StatusBadGateway)
	}

	s.sentryProxy = proxy
	s.sentryLimiter = newIPRateLimiter(rate.Every(200*time.Millisecond), 20, 5*time.Minute)
}

func (s *Server) buildSentryReportURI() string {
	if s.sentryProjectID == "" {
		return ""
	}

	pathPrefix := strings.TrimRight(sentryProxyPath, "/")
	if pathPrefix == "" {
		pathPrefix = ""
	}

	var b strings.Builder
	b.WriteString(pathPrefix)
	b.WriteString("/api/")
	b.WriteString(s.sentryProjectID)
	b.WriteString("/security/?sentry_version=7")
	if s.sentryPublicKey != "" {
		b.WriteString("&sentry_key=")
		b.WriteString(s.sentryPublicKey)
	}

	uri := b.String()
	if sentryReportHost != "" {
		return sentryReportHost + uri
	}
	return uri
}

func (s *Server) matchesStrippedSentryPath(path string) bool {
	if s.sentryProjectID == "" {
		return false
	}

	apiPath := "/api/" + s.sentryProjectID
	return path == apiPath || strings.HasPrefix(path, apiPath+"/")
}

func (s *Server) generateNonce() (string, error) {
	b := make([]byte, 16)
	if _, err := rand.Read(b); err != nil {
		return "", fmt.Errorf("generate nonce: %w", err)
	}
	return hex.EncodeToString(b), nil
}

func (s *Server) buildCSP(nonce string) string {
	cspHosts := map[string][]string{
		"FRAME": {
			"https://www.youtube.com/embed/",
			"https://www.youtube.com/s/player/",
			"https://hcaptcha.com",
			"https://*.hcaptcha.com",
			"https://challenges.cloudflare.com",
		},
		"IMAGE": {
			"https://*.fluxer.app",
			"https://i.ytimg.com",
			"https://*.youtube.com",
			"https://fluxerusercontent.com",
			"https://fluxerstatic.com",
			"https://*.fluxer.media",
			"https://fluxer.media",
			"http://127.0.0.1:21867",
			"http://127.0.0.1:21868",
		},
		"MEDIA": {
			"https://*.fluxer.app",
			"https://*.youtube.com",
			"https://fluxerusercontent.com",
			"https://fluxerstatic.com",
			"https://*.fluxer.media",
			"https://fluxer.media",
			"http://127.0.0.1:21867",
			"http://127.0.0.1:21868",
		},
		"SCRIPT": {
			"https://*.fluxer.app",
			"https://hcaptcha.com",
			"https://*.hcaptcha.com",
			"https://challenges.cloudflare.com",
			"https://fluxerstatic.com",
		},
		"STYLE": {
			"https://*.fluxer.app",
			"https://hcaptcha.com",
			"https://*.hcaptcha.com",
			"https://challenges.cloudflare.com",
			"https://fluxerstatic.com",
		},
		"FONT": {
			"https://*.fluxer.app",
			"https://fluxerstatic.com",
		},
		"CONNECT": {
			"https://*.fluxer.app",
			"wss://*.fluxer.app",
			"https://*.fluxer.media",
			"wss://*.fluxer.media",
			"https://hcaptcha.com",
			"https://*.hcaptcha.com",
			"https://challenges.cloudflare.com",
			"https://*.fluxer.workers.dev",
			"https://fluxerusercontent.com",
			"https://fluxerstatic.com",
			"https://sentry.web.fluxer.app",
			"https://sentry.web.canary.fluxer.app",
			"https://fluxer.media",
			"ipc:",
			"http://ipc.localhost",
			"http://127.0.0.1:21865",
			"ws://127.0.0.1:21865",
			"http://127.0.0.1:21866",
			"ws://127.0.0.1:21866",
			"http://127.0.0.1:21867",
			"http://127.0.0.1:21868",
			"http://127.0.0.1:21861",
			"http://127.0.0.1:21862",
			"http://127.0.0.1:21863",
			"http://127.0.0.1:21864",
		},
		"WORKER": {
			"https://*.fluxer.app",
			"https://fluxerstatic.com",
			"blob:",
		},
		"MANIFEST": {
			"https://*.fluxer.app",
		},
	}

	directives := []string{
		"default-src 'self'",
		fmt.Sprintf(
			"script-src 'self' 'nonce-%s' 'wasm-unsafe-eval' %s",
			nonce,
			strings.Join(cspHosts["SCRIPT"], " "),
		),
		"style-src 'self' 'unsafe-inline' " + strings.Join(cspHosts["STYLE"], " "),
		"img-src 'self' blob: data: " + strings.Join(cspHosts["IMAGE"], " "),
		"media-src 'self' blob: " + strings.Join(cspHosts["MEDIA"], " "),
		"font-src 'self' data: " + strings.Join(cspHosts["FONT"], " "),
		"connect-src 'self' data: " + strings.Join(cspHosts["CONNECT"], " "),
		"frame-src 'self' " + strings.Join(cspHosts["FRAME"], " "),
		"worker-src 'self' blob: " + strings.Join(cspHosts["WORKER"], " "),
		"manifest-src 'self' " + strings.Join(cspHosts["MANIFEST"], " "),
		"object-src 'none'",
		"base-uri 'self'",
		"frame-ancestors 'none'",
	}

	if uri := s.buildSentryReportURI(); uri != "" {
		directives = append(directives, fmt.Sprintf("report-uri %s", uri))
	}

	return strings.Join(directives, "; ")
}

func (s *Server) getRealIP(r *http.Request) string {
	xff := r.Header.Get("X-Forwarded-For")
	if xff == "" {
		return ""
	}
	ip := strings.TrimSpace(strings.Split(xff, ",")[0])
	ip = strings.Trim(ip, "[]")
	if net.ParseIP(ip) == nil {
		return ""
	}
	return ip
}

func (s *Server) getRateLimitKey(r *http.Request) string {
	if ip := s.getRealIP(r); ip != "" {
		return ip
	}
	host, _, err := net.SplitHostPort(r.RemoteAddr)
	if err == nil && host != "" {
		return host
	}

	return r.RemoteAddr
}

func (s *Server) handleIndex(w http.ResponseWriter) {
	nonce, err := s.generateNonce()
	if err != nil {
		s.errorLog.Printf("Failed to generate nonce: %v", err)
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Security-Policy", s.buildCSP(nonce))
	w.Header().Set("Content-Type", "text/html; charset=utf-8")

	indexBytes, err := assetsFS.ReadFile("assets/index.html")
	if err != nil {
		s.errorLog.Printf("Failed to read index.html: %v", err)
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}

	indexContent := strings.ReplaceAll(string(indexBytes), "{{CSP_NONCE_PLACEHOLDER}}", nonce)
	if _, err := w.Write([]byte(indexContent)); err != nil {
		s.errorLog.Printf("Failed to write response: %v", err)
	}
}

func (s *Server) handleStaticAsset(w http.ResponseWriter, r *http.Request, filename, contentType string) {
	data, err := assetsFS.ReadFile("assets/" + filename)
	if err != nil {
		s.errorLog.Printf("Failed to read %s: %v", filename, err)
		http.NotFound(w, r)
		return
	}

	w.Header().Set("Content-Type", contentType)
	if _, err := w.Write(data); err != nil {
		s.errorLog.Printf("Failed to write response for %s: %v", filename, err)
	}
}

func (s *Server) handleAssetsProxy(w http.ResponseWriter, r *http.Request) {
	if s.assetsProxy == nil {
		http.Error(w, "Assets proxy not configured", http.StatusInternalServerError)
		return
	}
	s.assetsProxy.ServeHTTP(w, r)
}

func (s *Server) handleSentryProxy(w http.ResponseWriter, r *http.Request) {
	if s.sentryProxy == nil {
		http.Error(w, "Sentry proxy not configured", http.StatusServiceUnavailable)
		return
	}

	key := s.getRateLimitKey(r)
	if s.sentryLimiter != nil && !s.sentryLimiter.Allow(key) {
		s.errorLog.Printf("Sentry proxy rate limited request from %s", key)
		http.Error(w, "Too Many Requests", http.StatusTooManyRequests)
		return
	}

	s.sentryProxy.ServeHTTP(w, r)
}

func (s *Server) logRequest(handler http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		requestID := fmt.Sprintf("%x", time.Now().UnixNano())

		w.Header().Set("X-Request-ID", requestID)

		wrapped := &responseWriter{
			ResponseWriter: w,
			status:         http.StatusOK,
		}

		realIP := s.getRealIP(r)

		s.accessLog.Printf("→ %s %s %s [%s] (IP: %s, UA: %s)",
			r.Method,
			r.URL.Path,
			r.Proto,
			requestID,
			realIP,
			r.UserAgent(),
		)

		handler(wrapped, r)

		duration := time.Since(start)
		recordProxyMetrics(r, wrapped, duration)

		duration = duration.Round(time.Millisecond)

		s.accessLog.Printf("← %s %d %s [%s] %dB %s",
			r.Method,
			wrapped.status,
			http.StatusText(wrapped.status),
			requestID,
			wrapped.size,
			duration,
		)
	}
}

func recordProxyMetrics(r *http.Request, rw *responseWriter, duration time.Duration) {
	status := rw.status
	if status == 0 {
		status = http.StatusOK
	}

	dims := map[string]string{
		"method": r.Method,
		"path":   r.URL.Path,
		"status": strconv.Itoa(status),
	}

	recordHistogram("/metrics/histogram", "app.proxy.latency", dims, float64(duration.Milliseconds()))
	recordCounter("/metrics/counter", "app.proxy.request", dims, 1)

	metric := "app.proxy.success"
	if status >= 400 {
		metric = "app.proxy.failure"
	}
	recordCounter("/metrics/counter", metric, dims, 1)
}

func recordHistogram(endpointPath, metric string, dimensions map[string]string, valueMs float64) {
	payload := map[string]any{
		"name":       metric,
		"dimensions": dimensions,
		"value_ms":   valueMs,
	}
	sendMetric(endpointPath, payload)
}

func recordCounter(endpointPath, metric string, dimensions map[string]string, value float64) {
	payload := map[string]any{
		"name":       metric,
		"dimensions": dimensions,
		"value":      value,
	}
	sendMetric(endpointPath, payload)
}

func sendMetric(path string, body map[string]any) {
	if metricsHost == "" {
		return
	}

	go func() {
		ctx, cancel := context.WithTimeout(context.Background(), 4*time.Second)
		defer cancel()

		data, err := json.Marshal(body)
		if err != nil {
			return
		}

		url := fmt.Sprintf("http://%s%s", metricsHost, path)
		req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(data))
		if err != nil {
			return
		}

		req.Header.Set("Content-Type", "application/json")
		req.Header.Set("User-Agent", "fluxer-proxy/metrics")

		resp, err := metricsClient.Do(req)
		if err != nil {
			return
		}
		io.Copy(io.Discard, resp.Body)
		resp.Body.Close()
	}()
}

func (s *Server) recoveryHandler(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if err := recover(); err != nil {
				s.errorLog.Printf("Panic recovered in %s %s: %v", r.Method, r.URL.Path, err)
				http.Error(w, "Internal Server Error", http.StatusInternalServerError)
			}
		}()
		next(w, r)
	}
}

func (s *Server) dispatch(w http.ResponseWriter, r *http.Request) {
	switch {
	case r.URL.Path == "/_health":
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		if _, err := w.Write([]byte("OK")); err != nil {
			s.errorLog.Printf("Failed to write health response: %v", err)
		}
		return

	case r.URL.Path == sentryProxyPath || strings.HasPrefix(r.URL.Path, sentryProxyPath+"/"):
		s.handleSentryProxy(w, r)
		return
	case s.matchesStrippedSentryPath(r.URL.Path):
		s.handleSentryProxy(w, r)
		return

	case r.URL.Path == "/assets" || strings.HasPrefix(r.URL.Path, "/assets/"):
		s.handleAssetsProxy(w, r)
		return

	case r.URL.Path == "/sw.js":
		s.handleStaticAsset(w, r, "sw.js", "application/javascript")
		return
	case r.URL.Path == "/sw.js.map":
		s.handleStaticAsset(w, r, "sw.js.map", "application/json")
		return
	case r.URL.Path == "/manifest.json":
		s.handleStaticAsset(w, r, "manifest.json", "application/manifest+json")
		return
	case r.URL.Path == "/version.json":
		s.handleStaticAsset(w, r, "version.json", "application/json")
		return
	case r.URL.Path == "/.well-known/apple-app-site-association":
		s.handleAppleAppSiteAssociation(w)
		return

	default:
		s.handleIndex(w)
	}
}

func (s *Server) handleAppleAppSiteAssociation(w http.ResponseWriter) {
	aasa := `{
  "webcredentials": {
    "apps": [
      "3G5837T29K.app.fluxer",
      "3G5837T29K.app.fluxer.canary"
    ]
  }
}`

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.Header().Set("Cache-Control", "public, max-age=3600")
	if _, err := w.Write([]byte(aasa)); err != nil {
		s.errorLog.Printf("Failed to write AASA response: %v", err)
	}
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	handler := s.recoveryHandler(s.logRequest(s.dispatch))
	handler(w, r)
}

func (s *Server) Start(port string) error {
	if port == "" {
		port = "8080"
	}

	addr := ":" + port
	s.httpServer = &http.Server{
		Addr:         addr,
		Handler:      s,
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 10 * time.Second,
		IdleTimeout:  120 * time.Second,
	}

	stop := make(chan os.Signal, 1)
	signal.Notify(stop, os.Interrupt, syscall.SIGTERM)

	errs := make(chan error, 1)

	go func() {
		s.accessLog.Printf("Starting server on %s", addr)
		if err := s.httpServer.ListenAndServe(); err != http.ErrServerClosed {
			errs <- err
		}
	}()

	select {
	case err := <-errs:
		return fmt.Errorf("server error: %w", err)
	case <-stop:
		s.accessLog.Printf("Shutting down server...")
		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		defer cancel()

		if err := s.httpServer.Shutdown(ctx); err != nil {
			return fmt.Errorf("server shutdown error: %w", err)
		}
	}

	return nil
}

func main() {
	server := NewServer()
	if err := server.Start(os.Getenv("PORT")); err != nil {
		server.errorLog.Fatal(err)
	}
}
