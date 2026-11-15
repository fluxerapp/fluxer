// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {getServedDocumentBranding} from '@app/features/app/state/ServedDocumentBranding';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {parseEndpoint} from '@app/features/messaging/utils/AttachmentCdnUrl';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';

interface InstanceBrandStyle {
	readonly themeColor: string | null;
	readonly customCss: string | null;
}

const NO_INSTANCE_BRAND_STYLE: InstanceBrandStyle = Object.freeze({themeColor: null, customCss: null});

export function getInstanceBrandStyle(): InstanceBrandStyle {
	const snapshot = RuntimeConfig.getSnapshotOrNull();
	if (snapshot === null) {
		return getServedDocumentBranding();
	}
	if (!(DeveloperOptions.selfHostedModeOverride || snapshot.features.self_hosted)) {
		return NO_INSTANCE_BRAND_STYLE;
	}
	return {themeColor: snapshot.appPublic.branding.theme_color, customCss: snapshot.appPublic.branding.custom_css};
}

export function getInstanceCssOrigins(): ReadonlyArray<string> | null {
	if (!isDesktop()) return null;
	const snapshot = RuntimeConfig.getSnapshotOrNull();
	if (snapshot === null) return [];
	const endpoints = [
		snapshot.mediaEndpoint,
		snapshot.staticCdnEndpoint,
		snapshot.apiPublicEndpoint,
		snapshot.webAppEndpoint,
	];
	return endpoints.flatMap((endpoint) => parseEndpoint(endpoint)?.origin ?? []);
}
