# SPDX-License-Identifier: AGPL-3.0-or-later
variable "BUILD_VERSION" { default = "" }
variable "PUBLIC_ASSET_BASE_URL" { default = "" }
variable "FLUXER_APP_PROXY_TIME_FREEZE_ENABLED" { default = "true" }
variable "BUNDLE_LOCAL_ASSETS" { default = "true" }
variable "IMAGE_REPO" { default = "" }
variable "CACHE_FROM" { default = "" }
variable "CACHE_TO" { default = "" }
variable "APP_ASSETS_REF" { default = "app-assets" }
variable "APP_ASSETS_PLATFORM" { default = "linux/amd64" }
variable "SOURCE_SHA" { default = "" }
variable "SOURCE_DATE" { default = "" }

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
		APP_ASSETS_REF                        = APP_ASSETS_REF
		APP_ASSETS_PLATFORM                   = APP_ASSETS_PLATFORM
		SOURCE_SHA                            = SOURCE_SHA
		SOURCE_DATE                           = SOURCE_DATE
	}
}

target "app-dist" {
	inherits = ["app-proxy"]
	target   = "app-dist"
	tags     = []
	output   = ["type=local,dest=app-dist-output"]
}

target "app-assets-image" {
	inherits = ["app-proxy"]
	target   = "app-assets"
	tags     = IMAGE_REPO != "" ? ["${IMAGE_REPO}:${BUILD_VERSION}-assets"] : []
	output   = ["type=registry"]
}
