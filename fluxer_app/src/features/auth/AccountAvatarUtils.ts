// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig, {runtimeConfigSnapshotsAreSameInstance} from '@app/features/app/state/RuntimeConfig';
import type {Account} from '@app/features/platform/state/AuthSession';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getDefaultAvatarAssetURL} from '@app/features/user/utils/DefaultAvatars';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import {useMemo} from 'react';

const LOCAL_ACCOUNT_AVATAR_DATA_URL_PATTERN = /^data:image\/(?:avif|gif|jpeg|png|webp);base64,[a-zA-Z0-9+/]+=*$/u;

function cachedAccountAvatarURL(account: Account, avatar: string, size?: MediaProxyImageSize): string | null {
	const userData = account.userData;
	if (!userData || userData.localAvatarHash !== avatar) {
		return null;
	}
	const localAvatarURL = userData.localAvatarURL;
	if (localAvatarURL == null || !LOCAL_ACCOUNT_AVATAR_DATA_URL_PATTERN.test(localAvatarURL)) {
		return null;
	}
	if (size != null && (typeof userData.localAvatarSize !== 'number' || userData.localAvatarSize < size)) {
		return null;
	}
	return localAvatarURL;
}

function accountInstanceMediaEndpoint(account: Account): string | null {
	const instance = account.instance;
	if (instance == null || typeof instance.mediaEndpoint !== 'string' || instance.mediaEndpoint.length === 0) {
		return null;
	}
	if (!runtimeConfigSnapshotsAreSameInstance(instance, RuntimeConfig.getSnapshotOrNull())) {
		return null;
	}
	return instance.mediaEndpoint;
}

function defaultAccountAvatarURL(account: Account, size?: MediaProxyImageSize): string {
	try {
		return AvatarUtils.getUserAvatarURL({id: account.userId, avatar: null}, false, size);
	} catch {
		return getDefaultAvatarAssetURL(0);
	}
}

function resolveAccountAvatarURL(account: Account, size?: MediaProxyImageSize): string {
	const avatar = account.userData?.avatar ?? null;
	if (avatar == null || avatar.length === 0) {
		return defaultAccountAvatarURL(account, size);
	}
	const cached = cachedAccountAvatarURL(account, avatar, size);
	if (cached != null) {
		return cached;
	}
	const mediaEndpoint = accountInstanceMediaEndpoint(account);
	if (mediaEndpoint == null) {
		return defaultAccountAvatarURL(account, size);
	}
	try {
		return AvatarUtils.getUserAvatarURLWithProxy({id: account.userId, avatar}, mediaEndpoint, false, size);
	} catch {
		return defaultAccountAvatarURL(account, size);
	}
}

export function useAccountAvatarURL(account: Account, size?: MediaProxyImageSize): string {
	return useMemo(() => resolveAccountAvatarURL(account, size), [account, size]);
}
