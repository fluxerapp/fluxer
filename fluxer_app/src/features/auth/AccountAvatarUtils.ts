// SPDX-License-Identifier: AGPL-3.0-or-later

import {useHover} from '@app/features/app/hooks/useHover';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import type {Account} from '@app/features/platform/state/AuthSession';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getDefaultAvatarURLForIndex, parseAvatarHash} from '@app/features/user/utils/AvatarUtils';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import type React from 'react';
import {useMemo} from 'react';

function defaultAccountAvatarURL(account: Account, size?: MediaProxyImageSize): string {
	try {
		return account.instance === undefined
			? AvatarUtils.getUserAvatarURL({id: account.userId, avatar: null}, false, size)
			: AvatarUtils.getDefaultAvatarURLForInstance(account.userId, account.instance);
	} catch {
		return getDefaultAvatarURLForIndex(0);
	}
}

function resolveAccountAvatarURL(account: Account, size: MediaProxyImageSize | undefined, animated: boolean): string {
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
			animated,
			size,
		);
	} catch {
		return defaultAccountAvatarURL(account, size);
	}
}

export function useAccountAvatarURL(account: Account, size?: MediaProxyImageSize): string {
	return useMemo(() => resolveAccountAvatarURL(account, size, false), [account, size]);
}

export function useAccountHoverAvatarURL(
	account: Account,
	size?: MediaProxyImageSize,
): [React.RefCallback<HTMLElement>, string] {
	const [hoverRef, isHovering] = useHover();
	const avatar = account.userData?.avatar ?? null;
	const animated = useShouldAnimate({
		kind: 'avatar',
		isAnimated: avatar != null && parseAvatarHash(avatar).animated,
		isHovering,
	});
	const avatarUrl = useMemo(() => resolveAccountAvatarURL(account, size, animated), [account, size, animated]);
	return [hoverRef, avatarUrl];
}
