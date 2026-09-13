// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	shouldShowNativeDesktopUpdateDownloadProgress,
	shouldShowNativeDesktopUpdateInApp,
} from '@app/features/app/utils/UpdaterPlatformUtils';
import {describe, expect, it} from 'vitest';

describe('UpdaterPlatformUtils', () => {
	it('surfaces native desktop update availability in-app on Linux, macOS, and Windows', () => {
		expect(shouldShowNativeDesktopUpdateInApp('linux')).toBe(true);
		expect(shouldShowNativeDesktopUpdateInApp('darwin')).toBe(true);
		expect(shouldShowNativeDesktopUpdateInApp('win32')).toBe(true);
	});
	it('does not surface native desktop update availability for unknown renderers', () => {
		expect(shouldShowNativeDesktopUpdateInApp(undefined)).toBe(false);
		expect(shouldShowNativeDesktopUpdateInApp(null)).toBe(false);
	});
	it('only surfaces native desktop download progress on Windows', () => {
		expect(shouldShowNativeDesktopUpdateDownloadProgress('win32')).toBe(true);
		expect(shouldShowNativeDesktopUpdateDownloadProgress('darwin')).toBe(false);
		expect(shouldShowNativeDesktopUpdateDownloadProgress('linux')).toBe(false);
		expect(shouldShowNativeDesktopUpdateDownloadProgress(undefined)).toBe(false);
	});
});
