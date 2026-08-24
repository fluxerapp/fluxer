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
import React, {type CSSProperties, useCallback, useEffect, useMemo, useState} from 'react';

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
	animateStatusCutout?: boolean;
	title?: never;
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
			animateStatusCutout = false,
			...props
		},
		ref,
	) => {
		const {i18n} = useLingui();
		const userId = user.id;
		const userAvatar = user.avatar;
		const guildMember = GuildMembers.getMember(guildId || '', userId);
		const hasGuildMemberAvatarSource = Boolean(guildId) && guildMember != null;
		const memberAvatar = guildMember?.avatar ?? null;
		const memberAvatarUnset = guildMember?.isAvatarUnset() ?? false;
		const avatarUrl = useMemo(() => {
			if (customAvatarUrl !== undefined) return customAvatarUrl;
			if (guildId && hasGuildMemberAvatarSource) {
				return AvatarUtils.getGuildMemberDisplayAvatarURL({
					guildId,
					user: {id: userId, avatar: userAvatar},
					memberAvatar,
					avatarUnset: memberAvatarUnset,
					animated: false,
					size: mediaSize,
				});
			}
			return AvatarUtils.getUserAvatarURL({id: userId, avatar: userAvatar}, false, mediaSize);
		}, [
			customAvatarUrl,
			guildId,
			hasGuildMemberAvatarSource,
			memberAvatar,
			memberAvatarUnset,
			mediaSize,
			userAvatar,
			userId,
		]);
		const hoverAvatarUrl = useMemo(() => {
			if (customHoverAvatarUrl !== undefined) return customHoverAvatarUrl;
			if (guildId && hasGuildMemberAvatarSource) {
				return AvatarUtils.getGuildMemberDisplayAvatarURL({
					guildId,
					user: {id: userId, avatar: userAvatar},
					memberAvatar,
					avatarUnset: memberAvatarUnset,
					animated: true,
					size: mediaSize,
				});
			}
			return AvatarUtils.getUserAvatarURL({id: userId, avatar: userAvatar}, true, mediaSize);
		}, [
			customHoverAvatarUrl,
			guildId,
			hasGuildMemberAvatarSource,
			memberAvatar,
			memberAvatarUnset,
			mediaSize,
			userAvatar,
			userId,
		]);
		const statusLabel = status != null ? getStatusTypeLabel(i18n, status) : null;
		const hasDistinctHoverAvatar = Boolean(hoverAvatarUrl && hoverAvatarUrl !== avatarUrl);
		const [hoverRef, isHovering] = useHover();
		const animationAllowed = useShouldAnimate({
			kind: 'avatar',
			isAnimated: hasDistinctHoverAvatar,
			isHovering: hasDistinctHoverAvatar && (isHovering || forceAnimate || forceAnimateIgnoringSettings),
		});
		const inlineAutoplayAllowed = animationAllowed && forceAnimateIgnoringSettings;
		const [requestedAnimatedUrl, setRequestedAnimatedUrl] = useState<string | null>(() =>
			hasDistinctHoverAvatar && animationAllowed ? hoverAvatarUrl : null,
		);
		useEffect(() => {
			if (hasDistinctHoverAvatar && animationAllowed) {
				setRequestedAnimatedUrl(hoverAvatarUrl);
			}
		}, [hasDistinctHoverAvatar, animationAllowed, hoverAvatarUrl]);
		const isAnimatedNeeded = hasDistinctHoverAvatar && (animationAllowed || requestedAnimatedUrl === hoverAvatarUrl);
		const rendersAnimatedInline = isAnimatedNeeded && inlineAutoplayAllowed;
		const [loadedStaticUrl, setLoadedStaticUrl] = useState<string | null>(null);
		const [loadedAnimatedUrl, setLoadedAnimatedUrl] = useState<string | null>(null);
		const isStaticLoaded = avatarUrl != null && loadedStaticUrl === avatarUrl;
		const isAnimatedLoaded = hasDistinctHoverAvatar && hoverAvatarUrl != null && loadedAnimatedUrl === hoverAvatarUrl;
		const handleImageLoaded = useCallback(
			(loadedUrl: string) => {
				if (loadedUrl === avatarUrl) setLoadedStaticUrl(loadedUrl);
				else if (loadedUrl === hoverAvatarUrl) setLoadedAnimatedUrl(loadedUrl);
			},
			[avatarUrl, hoverAvatarUrl],
		);
		useEffect(() => {
			if (!isAnimatedNeeded || rendersAnimatedInline || hoverAvatarUrl == null) {
				return;
			}
			let active = true;
			const cleanupAnimatedLoad = ImageCacheUtils.loadImage(hoverAvatarUrl, () => {
				if (active) setLoadedAnimatedUrl(hoverAvatarUrl);
			});
			return () => {
				active = false;
				cleanupAnimatedLoad();
			};
		}, [hoverAvatarUrl, isAnimatedNeeded, rendersAnimatedInline]);
		const shouldPlayAnimated = hasDistinctHoverAvatar && animationAllowed && isAnimatedLoaded;
		const fallbackAvatarUrl = useMemo(() => AvatarUtils.getUserAvatarURL({id: userId, avatar: null}, false), [userId]);
		const resolvedAvatarUrl = avatarUrl ?? fallbackAvatarUrl;
		const safeHoverAvatarUrl = isAnimatedNeeded ? hoverAvatarUrl || undefined : undefined;
		const normalizedStatusAttr = status != null ? normalizeStatus(status) : undefined;
		const displayName = NicknameUtils.getNickname(user, guildId ?? null);
		const avatarRefs = useMemo(() => [ref, hoverRef], [ref, hoverRef]);
		const mergedRef = useMergeRefs(avatarRefs);
		return (
			<BaseAvatar
				ref={mergedRef}
				size={size}
				avatarUrl={resolvedAvatarUrl}
				hoverAvatarUrl={safeHoverAvatarUrl}
				onImageLoaded={handleImageLoaded}
				status={status}
				isMobileStatus={isMobileStatus}
				showSkeleton
				shouldPlayAnimated={shouldPlayAnimated && isStaticLoaded}
				forceAnimatedPlayback={inlineAutoplayAllowed}
				isTyping={isTyping}
				showOffline={showOffline}
				className={className}
				isClickable={isClickable}
				userTag={NicknameUtils.formatTagForStreamerMode(user.tag)}
				statusLabel={statusLabel}
				disableStatusTooltip={disableStatusTooltip}
				animateStatusCutout={animateStatusCutout}
				data-flx="ui.avatar.avatar-component.base-avatar"
				data-flx-user-id={userId}
				data-flx-user-username={NicknameUtils.formatNameForStreamerMode(user.username)}
				data-flx-user-name={displayName}
				data-flx-user-bot={user.bot ? 'true' : undefined}
				data-flx-user-self={userId === Users.currentUserId ? 'true' : undefined}
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
