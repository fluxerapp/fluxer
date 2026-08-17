// SPDX-License-Identifier: AGPL-3.0-or-later

import {getStatusTypeLabel} from '@app/features/app/constants/AppConstants';
import {useHover} from '@app/features/app/hooks/useHover';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {BaseAvatar} from '@app/features/ui/components/BaseAvatar';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import {normalizeStatus} from '@fluxer/constants/src/StatusConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React, {type CSSProperties, useEffect, useMemo, useState} from 'react';

interface AvatarProps {
	user: User;
	size: number;
	status?: string | null;
	isMobileStatus?: boolean;
	forceAnimate?: boolean;
	forceAnimateIgnoringSettings?: boolean;
	isTyping?: boolean;
	showOffline?: boolean;
	className?: string;
	style?: CSSProperties;
	isClickable?: boolean;
	disableStatusTooltip?: boolean;
	avatarUrl?: string | null;
	hoverAvatarUrl?: string | null;
	guildId?: string | null;
	mediaSize?: MediaProxyImageSize;
	deferImageLoad?: boolean;
	animateStatusCutout?: boolean;
	title?: never;
}

type AvatarImagePresentation =
	| {
			kind: 'image';
			avatarUrl: string;
			showSkeleton: false;
	  }
	| {
			kind: 'deferredKnownAvatar';
			avatarUrl: '';
			showSkeleton: true;
	  };

function resolveInitiallyLoadedImageUrl(url: string | null): string | null {
	if (url == null) return null;
	if (ImageCacheUtils.hasImage(url)) return url;
	return null;
}

function resolveAvatarImagePresentation({
	avatarUrl,
	fallbackAvatarUrl,
	deferImageLoad,
	isStaticLoaded,
}: {
	avatarUrl: string | null;
	fallbackAvatarUrl: string;
	deferImageLoad: boolean;
	isStaticLoaded: boolean;
}): AvatarImagePresentation {
	const hasKnownAvatar = Boolean(avatarUrl) && avatarUrl !== fallbackAvatarUrl;
	if (deferImageLoad && !isStaticLoaded && hasKnownAvatar) {
		return {
			kind: 'deferredKnownAvatar',
			avatarUrl: '',
			showSkeleton: true,
		};
	}
	return {
		kind: 'image',
		avatarUrl: deferImageLoad && !isStaticLoaded ? fallbackAvatarUrl : (avatarUrl ?? fallbackAvatarUrl),
		showSkeleton: false,
	};
}

const AvatarComponent = React.forwardRef<HTMLDivElement, AvatarProps>(
	(
		{
			user,
			size,
			status,
			isMobileStatus = false,
			forceAnimate = false,
			forceAnimateIgnoringSettings = false,
			isTyping = false,
			showOffline = true,
			className,
			isClickable = false,
			disableStatusTooltip = false,
			avatarUrl: customAvatarUrl,
			hoverAvatarUrl: customHoverAvatarUrl,
			guildId,
			mediaSize,
			deferImageLoad = false,
			animateStatusCutout = false,
			...props
		},
		ref,
	) => {
		const {i18n} = useLingui();
		const guildMember = GuildMembers.getMember(guildId || '', user.id);
		const avatarUrl = useMemo(() => {
			if (customAvatarUrl !== undefined) return customAvatarUrl;
			if (guildId && guildMember) {
				return AvatarUtils.getGuildMemberDisplayAvatarURL({
					guildId,
					user,
					memberAvatar: guildMember.avatar,
					avatarUnset: guildMember.isAvatarUnset(),
					animated: false,
					size: mediaSize,
				});
			}
			return AvatarUtils.getUserAvatarURL(user, false, mediaSize);
		}, [user, customAvatarUrl, guildId, guildMember, mediaSize]);
		const hoverAvatarUrl = useMemo(() => {
			if (customHoverAvatarUrl !== undefined) return customHoverAvatarUrl;
			if (guildId && guildMember) {
				return AvatarUtils.getGuildMemberDisplayAvatarURL({
					guildId,
					user,
					memberAvatar: guildMember.avatar,
					avatarUnset: guildMember.isAvatarUnset(),
					animated: true,
					size: mediaSize,
				});
			}
			return AvatarUtils.getUserAvatarURL(user, true, mediaSize);
		}, [user, customHoverAvatarUrl, guildId, guildMember, mediaSize]);
		const statusLabel = status != null ? getStatusTypeLabel(i18n, status) : null;
		const hasDistinctHoverAvatar = Boolean(hoverAvatarUrl && hoverAvatarUrl !== avatarUrl);
		const [hoverRef, isHovering] = useHover();
		const settingsAnimationAllowed = useShouldAnimate({
			kind: 'avatar',
			isHovering: hasDistinctHoverAvatar && (isHovering || forceAnimate),
			respectPlaybackAllowed: hasDistinctHoverAvatar,
		});
		const animationAllowed = hasDistinctHoverAvatar && (forceAnimateIgnoringSettings || settingsAnimationAllowed);
		const [requestedAnimatedUrl, setRequestedAnimatedUrl] = useState<string | null>(() =>
			hasDistinctHoverAvatar && animationAllowed ? hoverAvatarUrl : null,
		);
		useEffect(() => {
			if (hasDistinctHoverAvatar && animationAllowed) {
				setRequestedAnimatedUrl(hoverAvatarUrl);
			}
		}, [hasDistinctHoverAvatar, animationAllowed, hoverAvatarUrl]);
		const isAnimatedNeeded = hasDistinctHoverAvatar && (animationAllowed || requestedAnimatedUrl === hoverAvatarUrl);
		const [loadedStaticUrl, setLoadedStaticUrl] = useState<string | null>(() =>
			resolveInitiallyLoadedImageUrl(avatarUrl),
		);
		const [loadedAnimatedUrl, setLoadedAnimatedUrl] = useState<string | null>(() =>
			resolveInitiallyLoadedImageUrl(hoverAvatarUrl),
		);
		const isStaticLoaded = avatarUrl != null && loadedStaticUrl === avatarUrl;
		const isAnimatedLoaded = hasDistinctHoverAvatar && hoverAvatarUrl != null && loadedAnimatedUrl === hoverAvatarUrl;
		useEffect(() => {
			const staticLoaded = ImageCacheUtils.hasImage(avatarUrl);
			const animatedLoaded = hasDistinctHoverAvatar ? ImageCacheUtils.hasImage(hoverAvatarUrl) : false;
			if (staticLoaded && loadedStaticUrl !== avatarUrl && avatarUrl != null) {
				setLoadedStaticUrl(avatarUrl);
			}
			if (animatedLoaded && loadedAnimatedUrl !== hoverAvatarUrl && hoverAvatarUrl != null) {
				setLoadedAnimatedUrl(hoverAvatarUrl);
			}
			if (deferImageLoad && !staticLoaded) {
				return;
			}
			let active = true;
			const cleanupStaticLoad = ImageCacheUtils.loadImage(avatarUrl, () => {
				if (active && avatarUrl != null) {
					setLoadedStaticUrl(avatarUrl);
				}
			});
			let cleanupAnimatedLoad: (() => void) | undefined;
			if (isAnimatedNeeded && !deferImageLoad && hoverAvatarUrl != null) {
				cleanupAnimatedLoad = ImageCacheUtils.loadImage(hoverAvatarUrl, () => {
					if (active) setLoadedAnimatedUrl(hoverAvatarUrl);
				});
			}
			return () => {
				active = false;
				cleanupStaticLoad();
				if (cleanupAnimatedLoad !== undefined) cleanupAnimatedLoad();
			};
		}, [avatarUrl, hoverAvatarUrl, hasDistinctHoverAvatar, isAnimatedNeeded, deferImageLoad]);
		const shouldPlayAnimated = hasDistinctHoverAvatar && animationAllowed && isAnimatedLoaded;
		const fallbackAvatarUrl = useMemo(
			() => AvatarUtils.getUserAvatarURL({id: user.id, avatar: null}, false),
			[user.id],
		);
		const avatarPresentation = resolveAvatarImagePresentation({
			avatarUrl,
			fallbackAvatarUrl,
			deferImageLoad,
			isStaticLoaded,
		});
		const safeHoverAvatarUrl = isAnimatedNeeded ? hoverAvatarUrl || undefined : undefined;
		const normalizedStatusAttr = status != null ? normalizeStatus(status) : undefined;
		const displayName = NicknameUtils.getNickname(user, guildId ?? null);
		const avatarRefs = useMemo(() => [ref, hoverRef], [ref, hoverRef]);
		const mergedRef = useMergeRefs(avatarRefs);
		return (
			<BaseAvatar
				ref={mergedRef}
				size={size}
				avatarUrl={avatarPresentation.avatarUrl}
				hoverAvatarUrl={safeHoverAvatarUrl}
				status={status}
				isMobileStatus={isMobileStatus}
				showSkeleton={avatarPresentation.showSkeleton}
				shouldPlayAnimated={shouldPlayAnimated && isStaticLoaded}
				forceAnimatedPlayback={forceAnimateIgnoringSettings}
				isTyping={isTyping}
				showOffline={showOffline}
				className={className}
				isClickable={isClickable}
				userTag={NicknameUtils.formatTagForStreamerMode(user.tag)}
				statusLabel={statusLabel}
				disableStatusTooltip={disableStatusTooltip}
				animateStatusCutout={animateStatusCutout}
				data-flx="ui.avatar.avatar-component.base-avatar"
				data-flx-user-id={user.id}
				data-flx-user-username={NicknameUtils.formatNameForStreamerMode(user.username)}
				data-flx-user-name={displayName}
				data-flx-user-bot={user.bot ? 'true' : undefined}
				data-flx-user-self={user.id === Users.currentUserId ? 'true' : undefined}
				data-flx-guild-id={guildId ?? undefined}
				data-flx-size={String(size)}
				data-flx-status={normalizedStatusAttr ?? undefined}
				data-flx-typing={isTyping ? 'true' : undefined}
				data-flx-clickable={isClickable ? 'true' : undefined}
				data-flx-mobile-status={isMobileStatus ? 'true' : undefined}
				{...props}
			/>
		);
	},
);

AvatarComponent.displayName = 'Avatar';

export const Avatar = observer(AvatarComponent);
