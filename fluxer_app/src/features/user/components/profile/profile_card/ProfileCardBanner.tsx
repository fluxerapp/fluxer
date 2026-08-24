// SPDX-License-Identifier: AGPL-3.0-or-later

import {useAnimatedImageUrl} from '@app/features/app/hooks/useAnimatedImageUrl';
import {GifIndicator} from '@app/features/channel/components/embeds/media/GifIndicator';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import styles from '@app/features/user/components/profile/profile_card/ProfileCardBanner.module.css';
import {
	PROFILE_POPOUT_BANNER_AVATAR_CUTOUT,
	PROFILE_POPOUT_GEOMETRY,
} from '@app/features/user/constants/UserProfileSurfaceGeometry';
import type {User} from '@app/features/user/models/User';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useId, useMemo} from 'react';

interface ProfileCardBannerProps {
	bannerUrl: string | null;
	hoverBannerUrl?: string | null;
	bannerColor: string;
	user: User;
	avatarUrl: string | null;
	hoverAvatarUrl: string | null;
	disablePresence?: boolean;
	isClickable?: boolean;
	onAvatarClick?: () => void;
	onAvatarContextMenu?: (event: React.MouseEvent) => void;
	onBannerContextMenu?: (event: React.MouseEvent) => void;
	headerHeight?: number;
}

export const ProfileCardBanner: React.FC<ProfileCardBannerProps> = observer(
	({
		bannerUrl,
		hoverBannerUrl,
		bannerColor,
		user,
		avatarUrl,
		hoverAvatarUrl,
		disablePresence = false,
		isClickable = true,
		onAvatarClick,
		onAvatarContextMenu,
		onBannerContextMenu,
		headerHeight = PROFILE_POPOUT_GEOMETRY.headerHeightPx,
	}) => {
		const bannerHeight = PROFILE_POPOUT_GEOMETRY.bannerHeightPx;
		const reactId = useId();
		const safeId = reactId.replace(/[^a-zA-Z0-9_-]/g, '');
		const maskId = `uid_${safeId}`;
		const headerStyle = useMemo<React.CSSProperties>(() => ({height: remFromPx(headerHeight)}), [headerHeight]);
		const bannerWrapperStyle = useMemo<React.CSSProperties>(
			() => ({minHeight: remFromPx(bannerHeight)}),
			[bannerHeight],
		);
		const {
			hoverRef: bannerHoverRef,
			imageUrl: activeBannerUrl,
			showGifIndicator,
		} = useAnimatedImageUrl({
			staticUrl: bannerUrl,
			animatedUrl: hoverBannerUrl,
			kind: 'gif',
		});
		const bannerStyle = useMemo<React.CSSProperties>(
			() => ({
				height: '100%',
				backgroundColor: bannerColor,
				...(activeBannerUrl ? {backgroundImage: `url(${activeBannerUrl})`} : {}),
			}),
			[activeBannerUrl, bannerColor],
		);
		return (
			<header
				className={styles.headerSection}
				style={headerStyle}
				data-flx="user.profile.profile-card.profile-card-banner.header-section"
			>
				<div
					ref={bannerHoverRef}
					role="group"
					className={styles.bannerWrapper}
					style={bannerWrapperStyle}
					onContextMenu={onBannerContextMenu}
					data-flx="user.profile.profile-card.profile-card-banner.banner-wrapper.banner-context-menu"
				>
					<svg
						className={styles.bannerMask}
						viewBox={`0 0 ${PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxWidthPx} ${PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}`}
						preserveAspectRatio="none"
						data-flx="user.profile.profile-card.profile-card-banner.banner-mask"
					>
						<mask id={maskId} data-flx="user.profile.profile-card.profile-card-banner.mask">
							<rect
								fill="white"
								x="0"
								y="0"
								width={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxWidthPx}
								height={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}
								data-flx="user.profile.profile-card.profile-card-banner.rect"
							/>
							<circle
								fill="black"
								cx={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.cx}
								cy={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.cy}
								r={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.r}
								data-flx="user.profile.profile-card.profile-card-banner.circle"
							/>
						</mask>
						<foreignObject
							x="0"
							y="0"
							width={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxWidthPx}
							height={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}
							overflow="visible"
							mask={`url(#${maskId})`}
							data-flx="user.profile.profile-card.profile-card-banner.foreign-object"
						>
							<div
								className={styles.banner}
								style={bannerStyle}
								data-flx="user.profile.profile-card.profile-card-banner.banner"
							/>
						</foreignObject>
					</svg>
					{showGifIndicator && <GifIndicator data-flx="user.profile.profile-card.profile-card-banner.gif-indicator" />}
				</div>
				<FocusRing offset={-2} data-flx="user.profile.profile-card.profile-card-banner.focus-ring">
					<button
						type="button"
						onClick={onAvatarClick}
						onContextMenu={onAvatarContextMenu}
						className={styles.avatarButton}
						data-flx="user.profile.profile-card.profile-card-banner.avatar-button.avatar-click"
					>
						<StatusAwareAvatar
							size={80}
							user={user}
							avatarUrl={avatarUrl}
							hoverAvatarUrl={hoverAvatarUrl}
							disablePresence={disablePresence}
							isClickable={isClickable}
							forceAnimate
							forceAnimateIgnoringSettings
							data-flx="user.profile.profile-card.profile-card-banner.status-aware-avatar"
						/>
					</button>
				</FocusRing>
			</header>
		);
	},
);
