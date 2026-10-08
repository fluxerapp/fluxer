// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import type {Account} from '@app/features/platform/state/AuthSession';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getDefaultAvatarAssetURL} from '@app/features/user/utils/DefaultAvatars';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import {useMemo} from 'react';

function defaultAccountAvatarURL(account: Account, size?: MediaProxyImageSize): string {
	try {
		return AvatarUtils.getUserAvatarURL({id: account.userId, avatar: null}, false, size);
	} catch {
		return getDefaultAvatarAssetURL(0);
	}
}

function resolveAccountAvatarURL(account: Account, size?: MediaProxyImageSize): string {
	const avatar = account.userData?.avatar ?? null;
	const instance = account.instance ?? null;
	const mediaEndpoint = instance?.mediaEndpoint ?? null;
	const instanceKey = instance == null ? null : runtimeInstanceKey(instance);
	if (
		avatar == null ||
		avatar.length === 0 ||
		mediaEndpoint == null ||
		mediaEndpoint.length === 0 ||
		instanceKey == null
	) {
		return defaultAccountAvatarURL(account, size);
	}
	try {
		return AvatarUtils.getUserAvatarURLForInstance(
			{id: account.userId, avatar},
			{mediaEndpoint, instanceKey},
			false,
			size,
		);
	} catch {
		return defaultAccountAvatarURL(account, size);
	}
}

export function useAccountAvatarURL(account: Account, size?: MediaProxyImageSize): string {
	return useMemo(() => resolveAccountAvatarURL(account, size), [account, size]);
}
