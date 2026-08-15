# SPDX-License-Identifier: AGPL-3.0-or-later
variable "BUILD_VERSION" { default = "" }
variable "PUBLIC_ASSET_BASE_URL" { default = "" }
variable "FLUXER_APP_PROXY_TIME_FREEZE_ENABLED" { default = "true" }
variable "BUNDLE_LOCAL_ASSETS" { default = "true" }
variable "IMAGE_REPO" { default = "" }
variable "CACHE_FROM" { default = "" }
variable "CACHE_TO" { default = "" }

group "default" {
	targets = ["app-proxy", "app-dist"]
}

target "app-proxy" {
	dockerfile = "fluxer_app_proxy/Dockerfile"
	context    = "."
	platforms  = ["linux/amd64"]
	tags       = IMAGE_REPO != "" ? ["${IMAGE_REPO}:${BUILD_VERSION}"] : []
	output     = ["type=registry"]
	cache-from = CACHE_FROM != "" ? [CACHE_FROM] : []
	cache-to   = CACHE_TO != "" ? [CACHE_TO] : []
	args = {
		BUILD_VERSION                         = BUILD_VERSION
		PUBLIC_ASSET_BASE_URL                 = PUBLIC_ASSET_BASE_URL
		FLUXER_APP_PROXY_TIME_FREEZE_ENABLED = FLUXER_APP_PROXY_TIME_FREEZE_ENABLED
		BUNDLE_LOCAL_ASSETS                   = BUNDLE_LOCAL_ASSETS
	}
}

target "app-dist" {
	inherits = ["app-proxy"]
	target   = "app-dist"
	tags     = []
	output   = ["type=local,dest=app-dist-output"]
}
