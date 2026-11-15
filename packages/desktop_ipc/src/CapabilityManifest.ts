// SPDX-License-Identifier: AGPL-3.0-or-later

export const DESKTOP_CAPABILITY_MANIFEST_CHANNEL = 'desktop-capability-manifest:get';

export interface DesktopCapabilityManifest {
	readonly appStore: boolean;
	readonly gatewaySocket: boolean;
}
