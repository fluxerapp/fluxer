// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAnimatedImageUrl} from '@app/features/app/hooks/useAnimatedImageUrl';
import {useMergeRefs} from '@app/features/app/hooks/useMergeRefs';
import {GifIndicator} from '@app/features/channel/components/embeds/media/GifIndicator';
import {
	getUserMenuAvatarUrl,
	getUserMenuBannerUrl,
	UserImageMenuItems,
} from '@app/features/ui/action_menu/items/UserImageMenuItems';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getAppRemScale} from '@app/features/ui/utils/AppZoomUtils';
import userProfileModalStyles from '@app/features/user/components/modals/UserProfileModal.module.css';
import {
	isKeyboardContextMenuTrigger,
	type ProfileMediaHeaderProps,
} from '@app/features/user/components/modals/user_profile_modal/UserProfileModalShared';
import {
	PROFILE_MODAL_BANNER_AVATAR_CUTOUT,
	PROFILE_MODAL_GEOMETRY,
	PROFILE_MODAL_GEOMETRY_STYLE,
} from '@app/features/user/constants/UserProfileSurfaceGeometry';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo, useState} from 'react';

const OPEN_BANNER_OPTIONS_DESCRIPTOR = msg({
	message: 'Open banner options',
	comment: 'Button or menu action label in the user profile modal. Keep it concise. Keep the tone plain and specific.',
});
const OPEN_AVATAR_OPTIONS_DESCRIPTOR = msg({
	message: 'Open avatar options',
	comment: 'Button or menu action label in the user profile modal. Keep it concise.',
});

const PROFILE_MODAL_DEFAULT_SCALE = 1.0666667;

function getProfileModalScale(bannerWidth: number | null): number {
	return (
		(bannerWidth ?? PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxWidthPx * PROFILE_MODAL_DEFAULT_SCALE) /
		PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxWidthPx
	);
}

export const ProfileMediaHeader: React.FC<ProfileMediaHeaderProps> = observer(
	({
		user,
		profile,
		profileContext,
		previewOverrides,
		bannerColor,
		bannerUrl,
		hoverBannerUrl,
		avatarUrl,
		hoverAvatarUrl,
		renderActionButtons,
	}) => {
		const {i18n} = useLingui();
		const reactId = useId();
		const safeId = reactId.replace(/[^a-zA-Z0-9_-]/g, '');
		const maskId = `uid_${safeId}`;
		const {
			hoverRef: bannerHoverRef,
			imageUrl: activeBannerUrl,
			showGifIndicator,
		} = useAnimatedImageUrl({
			staticUrl: bannerUrl,
			animatedUrl: hoverBannerUrl,
			kind: 'gif',
		});
		const [bannerElement, setBannerElement] = useState<HTMLDivElement | null>(null);
		const [bannerWidth, setBannerWidth] = useState<number | null>(null);
		const setMeasuredBannerElement = useCallback((node: HTMLDivElement | null) => {
			setBannerElement(node);
			if (!node) return;
			setBannerWidth(node.getBoundingClientRect().width);
		}, []);
		const mergedBannerRef = useMergeRefs<HTMLDivElement>([bannerHoverRef, setMeasuredBannerElement]);
		useEffect(() => {
			if (!bannerElement) return;
			const ownerWindow = bannerElement.ownerDocument.defaultView ?? window;
			const measure = () => setBannerWidth(bannerElement.getBoundingClientRect().width);
			measure();
			if (typeof ownerWindow.ResizeObserver === 'undefined') return;
			const resizeObserver = new ownerWindow.ResizeObserver((entries) => {
				const entry = entries[0];
				setBannerWidth(entry?.contentRect.width ?? bannerElement.getBoundingClientRect().width);
			});
			resizeObserver.observe(bannerElement);
			return () => resizeObserver.disconnect();
		}, [bannerElement]);
		const avatarScale = getProfileModalScale(bannerWidth);
		const avatarSize = PROFILE_MODAL_GEOMETRY.avatarSizePx * avatarScale;
		const avatarContainerStyle = useMemo<React.CSSProperties>(
			() => ({
				top: PROFILE_MODAL_GEOMETRY.avatarTopPx * avatarScale,
				left: PROFILE_MODAL_GEOMETRY.avatarLeftPx * avatarScale,
				borderWidth: PROFILE_MODAL_GEOMETRY.avatarBorderPx * avatarScale,
			}),
			[avatarScale],
		);
		const menuUrlOptions = {user, profile, profileContext, previewOverrides};
		const hasAvatarMenu = Boolean(getUserMenuAvatarUrl(menuUrlOptions));
		const hasBannerMenu = Boolean(getUserMenuBannerUrl(menuUrlOptions));
		const renderAvatarMenu = useCallback(
			({onClose}: {onClose: () => void}) => (
				<UserImageMenuItems
					user={user}
					profile={profile}
					profileContext={profileContext}
					previewOverrides={previewOverrides}
					onClose={onClose}
					variant="avatar"
					data-flx="user.user-profile-modal.render-avatar-menu.user-image-menu-items"
				/>
			),
			[user, profile, profileContext, previewOverrides],
		);
		const renderBannerMenu = useCallback(
			({onClose}: {onClose: () => void}) => (
				<UserImageMenuItems
					user={user}
					profile={profile}
					profileContext={profileContext}
					previewOverrides={previewOverrides}
					onClose={onClose}
					variant="banner"
					data-flx="user.user-profile-modal.render-banner-menu.user-image-menu-items"
				/>
			),
			[user, profile, profileContext, previewOverrides],
		);
		const handleImageContextMenu = useCallback(
			(
				event: React.MouseEvent<HTMLElement>,
				hasMenu: boolean,
				renderMenu: (props: {onClose: () => void}) => React.ReactNode,
			) => {
				if (!hasMenu) return;
				ContextMenuCommands.openFromEvent(event, renderMenu);
			},
			[],
		);
		const handleImageKeyDown = useCallback(
			(
				event: React.KeyboardEvent<HTMLElement>,
				hasMenu: boolean,
				renderMenu: (props: {onClose: () => void}) => React.ReactNode,
			) => {
				if (!hasMenu || !isKeyboardContextMenuTrigger(event)) return;
				event.preventDefault();
				event.stopPropagation();
				ContextMenuCommands.openForElement(event.currentTarget, renderMenu);
			},
			[],
		);
		const bannerContent = (
			<svg
				className={userProfileModalStyles.bannerMask}
				viewBox={`0 0 ${PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxWidthPx} ${PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}`}
				preserveAspectRatio="none"
				data-flx="user.user-profile-modal.profile-media-header.svg"
			>
				<mask id={maskId} data-flx="user.user-profile-modal.profile-media-header.mask">
					<rect
						fill="white"
						x="0"
						y="0"
						width={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxWidthPx}
						height={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}
						data-flx="user.user-profile-modal.profile-media-header.rect"
					/>
					<circle
						fill="black"
						cx={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.cx}
						cy={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.cy}
						r={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.r}
						data-flx="user.user-profile-modal.profile-media-header.circle"
					/>
				</mask>
				<foreignObject
					x="0"
					y="0"
					width={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxWidthPx}
					height={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}
					overflow="visible"
					mask={`url(#${maskId})`}
					data-flx="user.user-profile-modal.profile-media-header.foreign-object"
				>
					<div
						className={userProfileModalStyles.bannerImage}
						style={{
							backgroundColor: bannerColor,
							...(activeBannerUrl ? {backgroundImage: `url(${activeBannerUrl})`} : {}),
						}}
						data-flx="user.user-profile-modal.profile-media-header.div"
					/>
				</foreignObject>
			</svg>
		);
		const avatarContent = (
			<StatusAwareAvatar
				size={avatarSize / getAppRemScale()}
				user={user}
				avatarUrl={avatarUrl}
				hoverAvatarUrl={hoverAvatarUrl}
				forceAnimate
				forceAnimateIgnoringSettings
				data-flx="user.user-profile-modal.profile-media-header.status-aware-avatar"
			/>
		);
		return (
			<header style={PROFILE_MODAL_GEOMETRY_STYLE} data-flx="user.user-profile-modal.profile-media-header.header">
				<FocusRing enabled={hasBannerMenu} data-flx="user.user-profile-modal.profile-media-header.focus-ring">
					{/* biome-ignore lint/a11y/noStaticElementInteractions: the banner only takes role="button"/tabIndex when an image menu exists, and the element stays mounted either way so the banner image is never refetched. */}
					{/* biome-ignore lint/a11y/useAriaPropsSupportedByRole: aria-label is set only alongside role="button". */}
					<div
						ref={mergedBannerRef}
						className={userProfileModalStyles.bannerContainer}
						onContextMenu={
							hasBannerMenu ? (event) => handleImageContextMenu(event, hasBannerMenu, renderBannerMenu) : undefined
						}
						onKeyDown={
							hasBannerMenu ? (event) => handleImageKeyDown(event, hasBannerMenu, renderBannerMenu) : undefined
						}
						role={hasBannerMenu ? 'button' : undefined}
						tabIndex={hasBannerMenu ? 0 : undefined}
						aria-label={hasBannerMenu ? i18n._(OPEN_BANNER_OPTIONS_DESCRIPTOR) : undefined}
						data-flx="user.user-profile-modal.profile-media-header.button.image-key-down"
					>
						{bannerContent}
						{showGifIndicator && <GifIndicator data-flx="user.user-profile-modal.profile-media-header.gif-indicator" />}
					</div>
				</FocusRing>
				<div
					className={userProfileModalStyles.headerContainer}
					data-flx="user.user-profile-modal.profile-media-header.div--3"
				>
					<FocusRing
						enabled={hasAvatarMenu}
						ringClassName={userProfileModalStyles.avatarFocusRing}
						data-flx="user.user-profile-modal.profile-media-header.focus-ring--2"
					>
						{/* biome-ignore lint/a11y/noStaticElementInteractions: the avatar frame only takes role="button"/tabIndex when an image menu exists, and the element stays mounted either way so the avatar is never refetched. */}
						{/* biome-ignore lint/a11y/useAriaPropsSupportedByRole: aria-label is set only alongside role="button". */}
						<div
							className={userProfileModalStyles.avatarContainer}
							style={avatarContainerStyle}
							onContextMenu={
								hasAvatarMenu ? (event) => handleImageContextMenu(event, hasAvatarMenu, renderAvatarMenu) : undefined
							}
							onKeyDown={
								hasAvatarMenu ? (event) => handleImageKeyDown(event, hasAvatarMenu, renderAvatarMenu) : undefined
							}
							role={hasAvatarMenu ? 'button' : undefined}
							tabIndex={hasAvatarMenu ? 0 : undefined}
							aria-label={hasAvatarMenu ? i18n._(OPEN_AVATAR_OPTIONS_DESCRIPTOR) : undefined}
							data-flx="user.user-profile-modal.profile-media-header.button.image-key-down--2"
						>
							{avatarContent}
						</div>
					</FocusRing>
					<div
						className={userProfileModalStyles.actionButtonsContainer}
						data-flx="user.user-profile-modal.profile-media-header.div--5"
					>
						{renderActionButtons()}
					</div>
				</div>
			</header>
		);
	},
);
