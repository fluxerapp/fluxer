// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/user/components/profile/UserProfileLoadingSkeleton.module.css';
import {
	PROFILE_MODAL_BANNER_AVATAR_CUTOUT,
	PROFILE_MODAL_GEOMETRY_STYLE,
	PROFILE_POPOUT_BANNER_AVATAR_CUTOUT,
	PROFILE_POPOUT_GEOMETRY_STYLE,
} from '@app/features/user/constants/UserProfileSurfaceGeometry';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useId, useMemo} from 'react';

const LOADING_PROFILE_DESCRIPTOR = msg({
	message: 'Loading profile',
	comment: 'Accessible label for the user profile skeleton while profile data is loading.',
});

type UserProfileLoadingSkeletonVariant = 'popout' | 'modal';

interface UserProfileLoadingSkeletonProps {
	variant: UserProfileLoadingSkeletonVariant;
	borderColor?: string;

	[themeAttr: `data-flx${string}`]: string | undefined;
}

const PopoutBannerMask: React.FC<{maskId: string}> = ({maskId}) => (
	<svg
		className={styles.bannerMask}
		viewBox={`0 0 ${PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxWidthPx} ${PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}`}
		preserveAspectRatio="none"
		aria-hidden
		data-flx="user.profile.user-profile-loading-skeleton.popout-banner-mask.banner-mask"
	>
		<mask id={maskId} data-flx="user.profile.user-profile-loading-skeleton.popout-banner-mask.mask">
			<rect
				fill="white"
				x="0"
				y="0"
				width={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxWidthPx}
				height={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}
				data-flx="user.profile.user-profile-loading-skeleton.popout-banner-mask.rect"
			/>
			<circle
				fill="black"
				cx={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.cx}
				cy={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.cy}
				r={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.r}
				data-flx="user.profile.user-profile-loading-skeleton.popout-banner-mask.circle"
			/>
		</mask>
		<foreignObject
			x="0"
			y="0"
			width={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxWidthPx}
			height={PROFILE_POPOUT_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}
			overflow="visible"
			mask={`url(#${maskId})`}
			data-flx="user.profile.user-profile-loading-skeleton.popout-banner-mask.foreign-object"
		>
			<div
				className={styles.bannerFill}
				data-flx="user.profile.user-profile-loading-skeleton.popout-banner-mask.banner-fill"
			/>
		</foreignObject>
	</svg>
);

const ModalBannerMask: React.FC<{maskId: string}> = ({maskId}) => (
	<svg
		className={styles.bannerMask}
		viewBox={`0 0 ${PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxWidthPx} ${PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}`}
		preserveAspectRatio="none"
		aria-hidden
		data-flx="user.profile.user-profile-loading-skeleton.modal-banner-mask.banner-mask"
	>
		<mask id={maskId} data-flx="user.profile.user-profile-loading-skeleton.modal-banner-mask.mask">
			<rect
				fill="white"
				x="0"
				y="0"
				width={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxWidthPx}
				height={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}
				data-flx="user.profile.user-profile-loading-skeleton.modal-banner-mask.rect"
			/>
			<circle
				fill="black"
				cx={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.cx}
				cy={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.cy}
				r={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.r}
				data-flx="user.profile.user-profile-loading-skeleton.modal-banner-mask.circle"
			/>
		</mask>
		<foreignObject
			x="0"
			y="0"
			width={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxWidthPx}
			height={PROFILE_MODAL_BANNER_AVATAR_CUTOUT.viewBoxHeightPx}
			overflow="visible"
			mask={`url(#${maskId})`}
			data-flx="user.profile.user-profile-loading-skeleton.modal-banner-mask.foreign-object"
		>
			<div
				className={styles.bannerFill}
				data-flx="user.profile.user-profile-loading-skeleton.modal-banner-mask.banner-fill"
			/>
		</foreignObject>
	</svg>
);

const SkeletonLine: React.FC<{className?: string}> = ({className}) => (
	<div
		className={clsx(styles.line, className)}
		aria-hidden="true"
		data-flx="user.profile.user-profile-loading-skeleton.skeleton-line.line"
	/>
);

const PopoutSkeletonContent = () => (
	<>
		<div
			className={styles.content}
			data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.content"
		>
			<SkeletonLine
				className={styles.nameLine}
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.name-line"
			/>
			<SkeletonLine
				className={styles.usernameLine}
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.username-line"
			/>
			<SkeletonLine
				className={styles.statusLine}
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.status-line"
			/>
			<div
				className={clsx(styles.block, styles.activityBlock)}
				aria-hidden="true"
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.block"
			/>
			<div
				className={styles.section}
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section"
			>
				<SkeletonLine
					className={styles.sectionTitle}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section-title"
				/>
				<SkeletonLine
					className={styles.sectionLineLong}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section-line-long"
				/>
				<SkeletonLine
					className={styles.sectionLineMedium}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section-line-medium"
				/>
				<SkeletonLine
					className={styles.sectionLineShort}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section-line-short"
				/>
			</div>
			<div
				className={styles.section}
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section--2"
			>
				<SkeletonLine
					className={styles.sectionTitle}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section-title--2"
				/>
				<SkeletonLine
					className={styles.sectionLineMedium}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section-line-medium--2"
				/>
				<SkeletonLine
					className={styles.sectionLineShort}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.section-line-short--2"
				/>
			</div>
			<div
				className={styles.roleRow}
				aria-hidden="true"
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.role-row"
			>
				<div
					className={styles.pill}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.pill"
				/>
				<div
					className={styles.pill}
					style={{'--profile-skeleton-pill-width': remFromPx(58)} as React.CSSProperties}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.pill--2"
				/>
				<div
					className={styles.pill}
					style={{'--profile-skeleton-pill-width': remFromPx(72)} as React.CSSProperties}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.pill--3"
				/>
			</div>
			<div
				className={styles.connectionRow}
				aria-hidden="true"
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.connection-row"
			>
				<div
					className={styles.connectionPill}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.connection-pill"
				/>
				<div
					className={styles.connectionPill}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.connection-pill--2"
				/>
				<div
					className={styles.connectionPill}
					data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.connection-pill--3"
				/>
			</div>
		</div>
		<div className={styles.footer} data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.footer">
			<div
				className={styles.button}
				aria-hidden="true"
				data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content.button"
			/>
		</div>
	</>
);

const ModalSkeletonContent = () => (
	<div className={styles.content} data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.content">
		<div
			className={styles.modalUserInfo}
			data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.modal-user-info"
		>
			<SkeletonLine
				className={styles.modalNameLine}
				data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.modal-name-line"
			/>
			<SkeletonLine
				className={styles.modalTagLine}
				data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.modal-tag-line"
			/>
			<SkeletonLine
				className={styles.statusLine}
				data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.status-line"
			/>
		</div>
		<div
			className={styles.tabs}
			aria-hidden="true"
			data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.tabs"
		>
			<div className={styles.tab} data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.tab" />
			<div className={styles.tab} data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.tab--2" />
			<div className={styles.tab} data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.tab--3" />
		</div>
		<div className={styles.panel} data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.panel">
			<div
				className={clsx(styles.block, styles.activityBlock)}
				aria-hidden="true"
				data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.block"
			/>
			<div
				className={styles.section}
				data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section"
			>
				<SkeletonLine
					className={styles.sectionTitle}
					data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section-title"
				/>
				<SkeletonLine
					className={styles.sectionLineLong}
					data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section-line-long"
				/>
				<SkeletonLine
					className={styles.sectionLineMedium}
					data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section-line-medium"
				/>
				<SkeletonLine
					className={styles.sectionLineShort}
					data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section-line-short"
				/>
			</div>
			<div
				className={styles.mutualRow}
				data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.mutual-row"
			>
				<div
					className={styles.smallAvatar}
					aria-hidden="true"
					data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.small-avatar"
				/>
				<div
					className={styles.mutualText}
					data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.mutual-text"
				>
					<SkeletonLine
						className={styles.sectionLineMedium}
						data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section-line-medium--2"
					/>
					<SkeletonLine
						className={styles.sectionLineShort}
						data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section-line-short--2"
					/>
				</div>
			</div>
			<div
				className={styles.mutualRow}
				data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.mutual-row--2"
			>
				<div
					className={styles.smallAvatar}
					aria-hidden="true"
					data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.small-avatar--2"
				/>
				<div
					className={styles.mutualText}
					data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.mutual-text--2"
				>
					<SkeletonLine
						className={styles.sectionLineMedium}
						data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section-line-medium--3"
					/>
					<SkeletonLine
						className={styles.sectionLineShort}
						data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content.section-line-short--3"
					/>
				</div>
			</div>
		</div>
	</div>
);

export const UserProfileLoadingSkeleton: React.FC<UserProfileLoadingSkeletonProps> = observer(
	({variant, borderColor, ...props}) => {
		const {i18n} = useLingui();
		const reactId = useId();
		const maskId = `profile_skeleton_${reactId.replace(/[^a-zA-Z0-9_-]/g, '')}`;
		const shouldPulse = !Accessibility.useReducedMotion;
		const style = useMemo(
			() =>
				({
					...(variant === 'popout' ? PROFILE_POPOUT_GEOMETRY_STYLE : {}),
					...(variant === 'modal' ? PROFILE_MODAL_GEOMETRY_STYLE : {}),
					...(borderColor ? {'--profile-skeleton-border-color': borderColor} : {}),
				}) as React.CSSProperties,
			[borderColor, variant],
		);
		const pulseProps = shouldPulse
			? {
					animate: {opacity: [1, 0.65, 1]},
					transition: {duration: 1.4, ease: 'easeInOut' as const, repeat: Infinity},
				}
			: {};
		return (
			<div
				className={clsx(styles.surface, variant === 'modal' ? styles.modal : styles.popout)}
				style={style}
				role="status"
				aria-label={i18n._(LOADING_PROFILE_DESCRIPTOR)}
				data-flx="user.profile.user-profile-loading-skeleton.surface"
				{...props}
			>
				<motion.div
					className={styles.pulse}
					data-flx="user.profile.user-profile-loading-skeleton.pulse"
					{...pulseProps}
				>
					<div className={styles.header} data-flx="user.profile.user-profile-loading-skeleton.header">
						<div className={styles.banner} data-flx="user.profile.user-profile-loading-skeleton.banner">
							{variant === 'modal' ? (
								<ModalBannerMask
									maskId={maskId}
									data-flx="user.profile.user-profile-loading-skeleton.modal-banner-mask"
								/>
							) : (
								<PopoutBannerMask
									maskId={maskId}
									data-flx="user.profile.user-profile-loading-skeleton.popout-banner-mask"
								/>
							)}
							{variant === 'popout' && (
								<>
									<div
										className={styles.popoutAvatarSlot}
										aria-hidden="true"
										data-flx="user.profile.user-profile-loading-skeleton.popout-avatar-slot"
									/>
									<div
										className={styles.popoutAvatarStatus}
										aria-hidden="true"
										data-flx="user.profile.user-profile-loading-skeleton.popout-avatar-status"
									/>
								</>
							)}
						</div>
						<div className={styles.headerLower} data-flx="user.profile.user-profile-loading-skeleton.header-lower">
							{variant === 'modal' && (
								<div
									className={styles.avatar}
									aria-hidden="true"
									data-flx="user.profile.user-profile-loading-skeleton.avatar"
								/>
							)}
							{variant === 'modal' && (
								<div
									className={styles.modalActions}
									aria-hidden="true"
									data-flx="user.profile.user-profile-loading-skeleton.modal-actions"
								>
									<div
										className={styles.modalActionButton}
										data-flx="user.profile.user-profile-loading-skeleton.modal-action-button"
									/>
									<div
										className={styles.modalActionIcon}
										data-flx="user.profile.user-profile-loading-skeleton.modal-action-icon"
									/>
								</div>
							)}
						</div>
					</div>
					{variant === 'modal' ? (
						<ModalSkeletonContent data-flx="user.profile.user-profile-loading-skeleton.modal-skeleton-content" />
					) : (
						<PopoutSkeletonContent data-flx="user.profile.user-profile-loading-skeleton.popout-skeleton-content" />
					)}
				</motion.div>
			</div>
		);
	},
);
