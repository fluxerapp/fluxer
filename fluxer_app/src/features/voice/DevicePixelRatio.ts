// SPDX-License-Identifier: AGPL-3.0-or-later

export function resolveDevicePixelRatio(ownerWindow: Window | null): number {
	if (ownerWindow == null) {
		return 1;
	}
	const devicePixelRatio = ownerWindow.devicePixelRatio;
	if (!Number.isFinite(devicePixelRatio) || devicePixelRatio <= 0) {
		return 1;
	}
	return devicePixelRatio;
}
