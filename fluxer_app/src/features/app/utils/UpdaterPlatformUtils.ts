// SPDX-License-Identifier: AGPL-3.0-or-later

export function shouldShowNativeDesktopUpdateInApp(platform: string | null | undefined): boolean {
	return platform === 'darwin' || platform === 'win32' || platform === 'linux';
}

export function shouldShowNativeDesktopUpdateDownloadProgress(platform: string | null | undefined): boolean {
	return platform === 'win32' || platform === 'linux';
}
