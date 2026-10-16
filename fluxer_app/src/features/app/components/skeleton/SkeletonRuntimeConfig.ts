// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';

export function skeletonSingleCommunityEnabled(): boolean {
	return RuntimeConfig.getSnapshotOrNull()?.community.single_community ?? false;
}

export function skeletonDirectMessagesDisabled(): boolean {
	return RuntimeConfig.getSnapshotOrNull()?.community.direct_messages_disabled ?? false;
}

export function skeletonGifEnabled(): boolean {
	return RuntimeConfig.getSnapshotOrNull()?.services.gif_enabled ?? true;
}

export function skeletonSelfHosted(): boolean {
	return DeveloperOptions.selfHostedModeOverride || (RuntimeConfig.getSnapshotOrNull()?.features.self_hosted ?? false);
}
