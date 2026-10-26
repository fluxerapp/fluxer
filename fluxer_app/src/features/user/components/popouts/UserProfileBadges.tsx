// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {handleExternalLinkClick} from '@app/features/ui/utils/NativeUtils';
import styles from '@app/features/user/components/popouts/UserProfileBadges.module.css';
import {profileBadgeImageUrl} from '@app/features/user/utils/ProfileUtils';
import type {ProfileBadgeResponse} from '@fluxer/schema/src/domains/user/ProfileBadgeSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PencilSimpleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useState} from 'react';

interface UserProfileBadgesProps {
	badges: ReadonlyArray<ProfileBadgeResponse>;
	isModal?: boolean;
	isMobile?: boolean;
	onEditBadges?: () => void;
	'data-flx'?: string;
}

const EDIT_BADGES_DESCRIPTOR = msg({
	message: 'Edit badges',
	comment:
		'Tooltip and screen reader label for the pencil button next to your own profile badges. It opens the profile settings where you choose which badges other people see.',
});

const BADGE_REVEAL_TIMEOUT_MS = 1000;

function isSettledImage(url: string): boolean {
	return ImageCacheUtils.hasImage(url) || ImageCacheUtils.hasFailedImage(url);
}

function useBadgeImagesReady(badges: ReadonlyArray<ProfileBadgeResponse>): boolean {
	const key = badges.flatMap((badge) => profileBadgeImageUrl(badge) ?? []).join('\n');
	const settled = key === '' || key.split('\n').every(isSettledImage);
	const [readyKey, setReadyKey] = useState<string | null>(null);
	useEffect(() => {
		if (settled) return;
		const urls = key.split('\n');
		let pending = urls.length;
		const settle = () => {
			pending -= 1;
			if (pending === 0) setReadyKey(key);
		};
		const cancels = urls.map((url) => ImageCacheUtils.loadImage(url, settle, settle));
		const timer = window.setTimeout(() => setReadyKey(key), BADGE_REVEAL_TIMEOUT_MS);
		return () => {
			window.clearTimeout(timer);
			for (const cancel of cancels) cancel();
		};
	}, [key, settled]);
	return settled || readyKey === key;
}

function ProfileBadgeDetailsTooltip({
	badge,
	details,
	'data-flx': dataFlx,
}: {
	badge: ProfileBadgeResponse;
	details: NonNullable<ProfileBadgeResponse['details']>;
	'data-flx': string;
}) {
	const imageUrl = profileBadgeImageUrl(badge);
	return (
		<div className={styles.details} data-flx={dataFlx}>
			{imageUrl !== null && (
				<img
					src={imageUrl}
					alt=""
					className={styles.detailsArt}
					style={badge.aspect_ratio ? {width: 'auto', aspectRatio: badge.aspect_ratio} : undefined}
					data-flx={`${dataFlx}.art`}
				/>
			)}
			<span className={styles.detailsTitle} data-flx={`${dataFlx}.title`}>
				{details.title}
			</span>
			{details.lines.map((line) => (
				<span key={line} className={styles.detailsLine} data-flx={`${dataFlx}.line`}>
					{line}
				</span>
			))}
		</div>
	);
}

function opensPremiumModal(badge: ProfileBadgeResponse): boolean {
	if (badge.kind === 'premium') return badge.href === null;
	return badge.kind === 'plutonium';
}

export const UserProfileBadges: React.FC<UserProfileBadgesProps> = observer(
	({badges, isModal = false, isMobile = false, onEditBadges, 'data-flx': dataFlx = 'user.user-profile-badges'}) => {
		const {i18n} = useLingui();
		const ready = useBadgeImagesReady(badges);
		if (badges.length === 0 && !onEditBadges) {
			return null;
		}
		const containerClassName = isModal
			? clsx(styles.containerModal, isMobile ? styles.containerModalMobile : styles.containerModalDesktop)
			: styles.containerPopout;
		const touch = isModal && isMobile;
		const renderInteractiveWrapper = (badge: ProfileBadgeResponse, children: React.ReactNode) => {
			if (!isMobile && opensPremiumModal(badge)) {
				return (
					<button
						type="button"
						className={clsx(styles.link, styles.linkButton)}
						onClick={() => PremiumModalCommands.open()}
						data-flx={`${dataFlx}.button`}
					>
						{children}
					</button>
				);
			}
			const href = badge.href;
			if (href && !isMobile) {
				return (
					<a
						href={href}
						target="_blank"
						rel="noopener noreferrer"
						className={styles.link}
						onClick={(event) => handleExternalLinkClick(event, href)}
						data-flx={`${dataFlx}.link`}
					>
						{children}
					</a>
				);
			}
			return (
				<div className={styles.link} data-flx={`${dataFlx}.static`}>
					{children}
				</div>
			);
		};
		return (
			<div className={clsx(containerClassName, !ready && styles.pending)} data-flx={dataFlx}>
				{badges.map((badge) => {
					const imageUrl = profileBadgeImageUrl(badge);
					const content =
						imageUrl !== null ? (
							<img
								src={imageUrl}
								alt={badge.accessible_label ?? badge.caption ?? badge.label}
								className={touch ? styles.badgeMobile : styles.badgeDesktop}
								style={badge.aspect_ratio ? {width: 'auto', aspectRatio: badge.aspect_ratio} : undefined}
								data-flx={`${dataFlx}.img`}
							/>
						) : (
							<span
								className={clsx(styles.captionBadge, touch ? styles.captionBadgeMobile : styles.captionBadgeDesktop)}
								aria-hidden="true"
								data-flx={`${dataFlx}.caption`}
							>
								{badge.caption}
							</span>
						);
					const details = badge.details ?? null;
					const tooltipText =
						details && !touch
							? () => (
									<ProfileBadgeDetailsTooltip badge={badge} details={details} data-flx={`${dataFlx}.tooltip-details`} />
								)
							: badge.label;
					return (
						<Tooltip key={badge.key} text={tooltipText} maxWidth="xl" data-flx={`${dataFlx}.tooltip`}>
							<FocusRing offset={-2} data-flx={`${dataFlx}.focus-ring`}>
								{renderInteractiveWrapper(badge, content)}
							</FocusRing>
						</Tooltip>
					);
				})}
				{onEditBadges && (
					<Tooltip text={i18n._(EDIT_BADGES_DESCRIPTOR)} data-flx={`${dataFlx}.edit-tooltip`}>
						<FocusRing offset={-2} data-flx={`${dataFlx}.edit-focus-ring`}>
							<button
								type="button"
								className={clsx(styles.editButton, touch ? styles.badgeMobile : styles.badgeDesktop)}
								aria-label={i18n._(EDIT_BADGES_DESCRIPTOR)}
								onClick={onEditBadges}
								data-flx={`${dataFlx}.edit-button`}
							>
								<PencilSimpleIcon weight="bold" className={styles.editIcon} data-flx={`${dataFlx}.edit-icon`} />
							</button>
						</FocusRing>
					</Tooltip>
				)}
			</div>
		);
	},
);
