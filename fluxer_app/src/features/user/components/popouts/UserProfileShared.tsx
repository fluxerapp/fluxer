// SPDX-License-Identifier: AGPL-3.0-or-later

import {BLUESKY_PROVIDER_NAME, PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import {AddRoleButton, RoleList} from '@app/features/guild/components/RoleManagement';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {
	DOMAIN_DESCRIPTOR,
	OPEN_LINK_DESCRIPTOR,
	ROLES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getCachedDateTimeFormat} from '@app/features/i18n/utils/IntlCache';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {BlueskyIcon} from '@app/features/ui/components/icons/BlueskyIcon';
import {FluxerIcon} from '@app/features/ui/components/icons/FluxerIcon';
import {UnverifiedConnectionIcon} from '@app/features/ui/components/icons/UnverifiedConnectionIcon';
import {VerifiedConnectionIcon} from '@app/features/ui/components/icons/VerifiedConnectionIcon';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/popouts/UserProfileShared.module.css';
import type {Profile} from '@app/features/user/models/Profile';
import type {User} from '@app/features/user/models/User';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import type {UserProfile} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, GlobeSimpleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

const ABOUT_ME_DESCRIPTOR = msg({
	message: 'About me',
	comment: 'Short label in the user profile shared popout. Keep it concise.',
});
const THIS_CONNECTION_HAS_BEEN_VERIFIED_DESCRIPTOR = msg({
	message: 'This connection has been verified.',
	comment: 'Description text in the user profile shared popout.',
});
const THIS_CONNECTION_HAS_NOT_BEEN_VERIFIED_DESCRIPTOR = msg({
	message: 'This connection has not been verified.',
	comment: 'Description text in the user profile shared popout.',
});
const PRODUCT_MEMBER_SINCE_DESCRIPTOR = msg({
	message: '{productName} member since',
	comment: 'Profile membership label showing when the user account was created.',
});
const LOCAL_TIME_DESCRIPTOR = msg({
	message: 'Local time',
	comment: "Profile section title for a user's local time.",
});
const SAME_TIME_AS_YOU_DESCRIPTOR = msg({
	message: 'Same time as you',
	comment: "Profile timezone difference text. The target user's timezone currently matches the viewer's timezone.",
});
const TIME_AHEAD_OF_YOU_DESCRIPTOR = msg({
	message: '{duration} ahead of you',
	comment:
		"Profile timezone difference text. duration is a localized hours/minutes phrase for how far ahead the target user's local time is.",
});
const TIME_BEHIND_YOU_DESCRIPTOR = msg({
	message: '{duration} behind you',
	comment:
		"Profile timezone difference text. duration is a localized hours/minutes phrase for how far behind the target user's local time is.",
});
const HOURS_MINUTES_DURATION_DESCRIPTOR = msg({
	message: '{hours, plural, one {# hour} other {# hours}} {minutes, plural, one {# minute} other {# minutes}}',
	comment: 'Duration phrase in the profile timezone section. hours and minutes are positive integers.',
});
const HOURS_DURATION_DESCRIPTOR = msg({
	message: '{hours, plural, one {# hour} other {# hours}}',
	comment: 'Duration phrase in the profile timezone section. hours is a positive integer.',
});
const MINUTES_DURATION_DESCRIPTOR = msg({
	message: '{minutes, plural, one {# minute} other {# minutes}}',
	comment: 'Duration phrase in the profile timezone section. minutes is a positive integer.',
});

function formatOffsetDifferenceDuration(i18n: I18n, minutes: number): string {
	const absolute = Math.abs(minutes);
	const hours = Math.floor(absolute / 60);
	const remainingMinutes = absolute % 60;
	if (hours > 0 && remainingMinutes > 0) {
		return i18n._(HOURS_MINUTES_DURATION_DESCRIPTOR, {hours, minutes: remainingMinutes});
	}
	if (hours > 0) {
		return i18n._(HOURS_DURATION_DESCRIPTOR, {hours});
	}
	return i18n._(MINUTES_DURATION_DESCRIPTOR, {minutes: remainingMinutes});
}

function getViewerOffsetMinutes(): number {
	return -new Date().getTimezoneOffset();
}

function getDateAtOffset(now: Date, offsetMinutes: number): Date {
	return new Date(now.getTime() + offsetMinutes * 60_000);
}

export const UserProfileBio: React.FC<{
	profile: Profile;
	profileData?: Readonly<UserProfile> | null;
	onShowMore?: () => void;
}> = observer(({profile, profileData, onShowMore}) => {
	const {i18n} = useLingui();
	const resolvedProfile = profileData ?? profile?.getEffectiveProfile() ?? null;
	const bioContent = resolvedProfile?.bio ?? '';
	const shouldTruncate = !!onShowMore;
	const bioRef = useRef<HTMLDivElement | null>(null);
	const [isBioTruncated, setIsBioTruncated] = useState(false);
	const checkBioTruncation = useCallback(() => {
		if (!shouldTruncate || !bioContent) {
			setIsBioTruncated(false);
			return;
		}
		const bioElement = bioRef.current;
		if (!bioElement) {
			setIsBioTruncated(false);
			return;
		}
		const isOverflowingHeight = bioElement.scrollHeight - bioElement.clientHeight > 1;
		const isOverflowingWidth = bioElement.scrollWidth - bioElement.clientWidth > 1;
		setIsBioTruncated(isOverflowingHeight || isOverflowingWidth);
	}, [bioContent, shouldTruncate]);
	useLayoutEffect(() => {
		checkBioTruncation();
	}, [checkBioTruncation]);
	useEffect(() => {
		if (!shouldTruncate || !bioContent) {
			return;
		}
		const bioElement = bioRef.current;
		if (!bioElement || typeof ResizeObserver === 'undefined') {
			return;
		}
		let frameId: number | null = null;
		const scheduleCheck = () => {
			if (frameId != null) {
				return;
			}
			frameId = requestAnimationFrame(() => {
				frameId = null;
				checkBioTruncation();
			});
		};
		const resizeObserver = new ResizeObserver(scheduleCheck);
		resizeObserver.observe(bioElement);
		return () => {
			if (frameId != null) {
				cancelAnimationFrame(frameId);
			}
			resizeObserver.disconnect();
		};
	}, [bioContent, checkBioTruncation, shouldTruncate]);
	if (!bioContent) {
		return null;
	}
	return (
		<section
			className={styles.bioContainer}
			aria-label={i18n._(ABOUT_ME_DESCRIPTOR)}
			data-flx="user.user-profile-shared.user-profile-bio.bio-container"
		>
			<div
				ref={bioRef}
				className={clsx(markupStyles.markup, markupStyles.bio, markupStyles.mutedSpoilerContext, {
					[styles.bioClamped]: shouldTruncate,
				})}
				data-flx="user.user-profile-shared.user-profile-bio.bio-clamped"
			>
				<SafeMarkdown
					content={bioContent}
					options={{context: MarkdownContext.RESTRICTED_USER_BIO, guildId: profile?.guildId ?? undefined}}
					data-flx="user.user-profile-shared.user-profile-bio.safe-markdown"
				/>
			</div>
			{shouldTruncate && isBioTruncated && (
				<FocusRing offset={-2} data-flx="user.user-profile-shared.user-profile-bio.focus-ring">
					<button
						type="button"
						onClick={onShowMore}
						className={styles.viewFullButton}
						data-flx="user.user-profile-shared.user-profile-bio.view-full-button.show-more"
					>
						<Trans>View full profile</Trans>
					</button>
				</FocusRing>
			)}
		</section>
	);
});

interface UserProfilePreviewBioProps {
	profile: Profile;
	profileData?: Readonly<UserProfile> | null;
	onShowMore: () => void;
}

export const UserProfilePreviewBio: React.FC<UserProfilePreviewBioProps> = ({profile, profileData, onShowMore}) => {
	return (
		<UserProfileBio
			profile={profile}
			profileData={profileData}
			onShowMore={onShowMore}
			data-flx="user.user-profile-shared.user-profile-preview-bio.user-profile-bio"
		/>
	);
};
export const UserProfileTimezoneInfo: React.FC<{profile: Profile}> = observer(({profile}) => {
	const {i18n} = useLingui();
	const [now, setNow] = useState(() => new Date());
	const timezoneOffset = profile.timezoneOffset;
	useEffect(() => {
		if (timezoneOffset == null) {
			return;
		}
		const interval = window.setInterval(() => setNow(new Date()), 30_000);
		return () => window.clearInterval(interval);
	}, [timezoneOffset]);
	if (timezoneOffset == null) {
		return null;
	}
	const locale = getCurrentLocale();
	const localTime = getCachedDateTimeFormat(locale, {
		hour: 'numeric',
		minute: '2-digit',
		timeZone: 'UTC',
	}).format(getDateAtOffset(now, timezoneOffset));
	const offsetDifference = timezoneOffset - getViewerOffsetMinutes();
	const differenceText =
		offsetDifference === 0
			? i18n._(SAME_TIME_AS_YOU_DESCRIPTOR)
			: offsetDifference > 0
				? i18n._(TIME_AHEAD_OF_YOU_DESCRIPTOR, {
						duration: formatOffsetDifferenceDuration(i18n, offsetDifference),
					})
				: i18n._(TIME_BEHIND_YOU_DESCRIPTOR, {
						duration: formatOffsetDifferenceDuration(i18n, offsetDifference),
					});
	return (
		<div className={styles.timeZoneContainer} data-flx="user.user-profile-shared.user-profile-timezone-info.container">
			<span className={styles.timeZoneTitle} data-flx="user.user-profile-shared.user-profile-timezone-info.title">
				{i18n._(LOCAL_TIME_DESCRIPTOR)}
			</span>
			<span className={styles.timeZoneTime} data-flx="user.user-profile-shared.user-profile-timezone-info.time">
				{localTime}
			</span>
			<span
				className={styles.timeZoneDifference}
				data-flx="user.user-profile-shared.user-profile-timezone-info.difference"
			>
				{differenceText}
			</span>
		</div>
	);
});
export const UserProfileMembershipInfo: React.FC<{profile: Profile; user: User}> = observer(({profile, user}) => {
	const {i18n} = useLingui();
	if (profile?.guild && profile.guildMember) {
		return (
			<div
				className={styles.membershipContainer}
				data-flx="user.user-profile-shared.user-profile-membership-info.membership-container"
			>
				<span
					className={styles.membershipTitle}
					data-flx="user.user-profile-shared.user-profile-membership-info.membership-title"
				>
					<Trans>Member since</Trans>
				</span>
				<div
					className={styles.membershipDates}
					data-flx="user.user-profile-shared.user-profile-membership-info.membership-dates"
				>
					<div
						className={styles.membershipDate}
						data-flx="user.user-profile-shared.user-profile-membership-info.membership-date"
					>
						<Tooltip text={PRODUCT_NAME} data-flx="user.user-profile-shared.user-profile-membership-info.tooltip">
							<div
								className={styles.membershipIcon}
								data-flx="user.user-profile-shared.user-profile-membership-info.membership-icon"
							>
								<FluxerIcon
									className={clsx(styles.iconSmall, styles.textChat)}
									data-flx="user.user-profile-shared.user-profile-membership-info.icon-small"
								/>
							</div>
						</Tooltip>
						<span
							className={styles.membershipDateText}
							data-flx="user.user-profile-shared.user-profile-membership-info.membership-date-text"
						>
							{DateUtils.getFormattedShortDate(user.createdAt)}
						</span>
					</div>
					<div
						className={styles.membershipDate}
						data-flx="user.user-profile-shared.user-profile-membership-info.membership-date--2"
					>
						<Tooltip
							text={profile.guild.name}
							data-flx="user.user-profile-shared.user-profile-membership-info.tooltip--2"
						>
							<div
								className={styles.membershipIcon}
								data-flx="user.user-profile-shared.user-profile-membership-info.membership-icon--2"
							>
								<GuildIcon
									id={profile.guild.id}
									name={profile.guild.name}
									icon={profile.guild.icon}
									className={clsx(styles.membershipGuildIcon, styles.textXs)}
									sizePx={16}
									data-flx="user.user-profile-shared.user-profile-membership-info.membership-guild-icon"
								/>
							</div>
						</Tooltip>
						<span
							className={styles.membershipDateText}
							data-flx="user.user-profile-shared.user-profile-membership-info.membership-date-text--2"
						>
							{DateUtils.getFormattedShortDate(profile.guildMember.joinedAt)}
						</span>
					</div>
				</div>
			</div>
		);
	}
	return (
		<div
			className={styles.membershipContainer}
			data-flx="user.user-profile-shared.user-profile-membership-info.membership-container--2"
		>
			<span
				className={styles.membershipTitle}
				data-flx="user.user-profile-shared.user-profile-membership-info.membership-title--2"
			>
				{i18n._(PRODUCT_MEMBER_SINCE_DESCRIPTOR, {productName: PRODUCT_NAME})}
			</span>
			<span
				className={styles.membershipDateText}
				data-flx="user.user-profile-shared.user-profile-membership-info.membership-date-text--3"
			>
				{DateUtils.getFormattedShortDate(user.createdAt)}
			</span>
		</div>
	);
});
export const UserProfileRoles: React.FC<{
	profile: Profile;
	user: User;
	memberRoles: Array<GuildRole>;
	canManageRoles: boolean;
}> = observer(({profile, user, memberRoles, canManageRoles}) => {
	const {i18n} = useLingui();
	return profile?.guild && profile?.guildMember && (memberRoles.length > 0 || canManageRoles) ? (
		<div className={styles.rolesContainer} data-flx="user.user-profile-shared.user-profile-roles.roles-container">
			<div className={styles.rolesHeader} data-flx="user.user-profile-shared.user-profile-roles.roles-header">
				<span className={styles.rolesTitle} data-flx="user.user-profile-shared.user-profile-roles.roles-title">
					{i18n._(ROLES_DESCRIPTOR)}
				</span>
				{canManageRoles && (
					<AddRoleButton
						guildId={profile.guild.id}
						userId={user.id}
						data-flx="user.user-profile-shared.user-profile-roles.add-role-button"
					/>
				)}
			</div>
			{memberRoles.length > 0 ? (
				<RoleList
					guildId={profile.guild.id}
					userId={user.id}
					roles={memberRoles}
					canManage={canManageRoles}
					data-flx="user.user-profile-shared.user-profile-roles.role-list"
				/>
			) : (
				<span className={styles.rolesEmpty} data-flx="user.user-profile-shared.user-profile-roles.roles-empty">
					<Trans>This user has no roles in this community.</Trans>
				</span>
			)}
		</div>
	) : null;
});

function getConnectionUrl(type: string, name: string): string {
	return type === ConnectionTypes.BLUESKY ? `https://bsky.app/profile/${name}` : `https://${name}`;
}

const ConnectionCard: React.FC<{
	connection: {id: string; type: string; name: string; verified: boolean};
	onLinkClick: (e: React.MouseEvent<HTMLAnchorElement>, url: string) => void;
	mobile?: boolean;
}> = ({connection, onLinkClick, mobile}) => {
	const {i18n} = useLingui();
	const url = getConnectionUrl(connection.type, connection.name);
	const iconLabel = connection.type === ConnectionTypes.BLUESKY ? BLUESKY_PROVIDER_NAME : i18n._(DOMAIN_DESCRIPTOR);
	const icon = (
		<Tooltip text={iconLabel} data-flx="user.user-profile-shared.connection-card.tooltip">
			<div className={styles.connectionIcon} data-flx="user.user-profile-shared.connection-card.connection-icon">
				{connection.type === ConnectionTypes.BLUESKY ? (
					<BlueskyIcon size={18} data-flx="user.user-profile-shared.connection-card.bluesky-icon" />
				) : (
					<GlobeSimpleIcon
						size={remFromPx(18)}
						className={styles.connectionDomainIcon}
						data-flx="user.user-profile-shared.connection-card.connection-domain-icon"
					/>
				)}
			</div>
		</Tooltip>
	);
	const nameRow = (
		<div
			className={styles.connectionCardNameRow}
			data-flx="user.user-profile-shared.connection-card.connection-card-name-row"
		>
			<span
				className={styles.connectionCardName}
				data-flx="user.user-profile-shared.connection-card.connection-card-name"
			>
				{connection.name}
			</span>
			<Tooltip
				text={
					connection.verified
						? i18n._(THIS_CONNECTION_HAS_BEEN_VERIFIED_DESCRIPTOR)
						: i18n._(THIS_CONNECTION_HAS_NOT_BEEN_VERIFIED_DESCRIPTOR)
				}
				data-flx="user.user-profile-shared.connection-card.tooltip--2"
			>
				<div className={styles.connectionBadge} data-flx="user.user-profile-shared.connection-card.connection-badge">
					{connection.verified ? (
						<VerifiedConnectionIcon
							size={16}
							data-flx="user.user-profile-shared.connection-card.verified-connection-icon"
						/>
					) : (
						<UnverifiedConnectionIcon
							size={16}
							data-flx="user.user-profile-shared.connection-card.unverified-connection-icon"
						/>
					)}
				</div>
			</Tooltip>
		</div>
	);
	if (mobile) {
		return (
			<a
				href={url}
				target="_blank"
				rel="noopener noreferrer"
				className={styles.connectionCard}
				onClick={(e) => onLinkClick(e, url)}
				data-flx="user.user-profile-shared.connection-card.connection-card.link-click"
			>
				{icon}
				{nameRow}
				<ArrowSquareOutIcon
					size={remFromPx(16)}
					weight="bold"
					className={styles.connectionExternalArrow}
					data-flx="user.user-profile-shared.connection-card.connection-external-arrow"
				/>
			</a>
		);
	}
	return (
		<div className={styles.connectionCard} data-flx="user.user-profile-shared.connection-card.connection-card">
			{icon}
			{nameRow}
			<Tooltip text={i18n._(OPEN_LINK_DESCRIPTOR)} data-flx="user.user-profile-shared.connection-card.tooltip--3">
				<div data-flx="user.user-profile-shared.connection-card.div">
					<a
						href={url}
						target="_blank"
						rel="noopener noreferrer"
						className={styles.connectionExternalLink}
						onClick={(e) => onLinkClick(e, url)}
						data-flx="user.user-profile-shared.connection-card.connection-external-link.link-click"
					>
						<ArrowSquareOutIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="user.user-profile-shared.connection-card.arrow-square-out-icon"
						/>
					</a>
				</div>
			</Tooltip>
		</div>
	);
};
export const UserProfileConnections: React.FC<{
	profile: Profile;
	variant?: 'compact' | 'cards' | 'mobile';
}> = observer(({profile, variant}) => {
	const handleConnectionClick = useCallback((e: React.MouseEvent<HTMLAnchorElement>, url: string) => {
		e.preventDefault();
		e.stopPropagation();
		openExternalUrlWithWarning(url);
	}, []);
	if (StreamerMode.shouldHidePersonalInformation) {
		return null;
	}
	const connections = profile.connectedAccounts;
	if (!connections || connections.length === 0) {
		return null;
	}
	if (variant === 'compact') {
		return (
			<div
				className={styles.connectionsCompactWrapper}
				data-flx="user.user-profile-shared.user-profile-connections.connections-compact-wrapper"
			>
				<div
					className={styles.connectionsCompactSeparator}
					data-flx="user.user-profile-shared.user-profile-connections.connections-compact-separator"
				/>
				<div
					className={styles.connectionsCompact}
					data-flx="user.user-profile-shared.user-profile-connections.connections-compact"
				>
					{connections.map((connection) => {
						const url = getConnectionUrl(connection.type, connection.name);
						return (
							<Tooltip
								key={connection.id}
								text={() => (
									<span
										className={styles.connectionTooltipContent}
										data-flx="user.user-profile-shared.user-profile-connections.connection-tooltip-content"
									>
										<span
											className={styles.connectionTooltipName}
											data-flx="user.user-profile-shared.user-profile-connections.span"
										>
											{connection.name}
										</span>
										{connection.verified ? (
											<VerifiedConnectionIcon
												size={14}
												className={styles.connectionTooltipBadgeIcon}
												data-flx="user.user-profile-shared.user-profile-connections.verified-connection-icon"
											/>
										) : (
											<UnverifiedConnectionIcon
												size={14}
												className={styles.connectionTooltipBadgeIcon}
												data-flx="user.user-profile-shared.user-profile-connections.unverified-connection-icon"
											/>
										)}
									</span>
								)}
								data-flx="user.user-profile-shared.user-profile-connections.tooltip"
							>
								<div data-flx="user.user-profile-shared.user-profile-connections.div">
									<a
										href={url}
										target="_blank"
										rel="noopener noreferrer"
										className={styles.connectionCompactIcon}
										onClick={(e) => handleConnectionClick(e, url)}
										data-flx="user.user-profile-shared.user-profile-connections.connection-compact-icon.connection-click"
									>
										{connection.type === ConnectionTypes.BLUESKY ? (
											<BlueskyIcon
												size={16}
												data-flx="user.user-profile-shared.user-profile-connections.bluesky-icon"
											/>
										) : (
											<GlobeSimpleIcon
												size={remFromPx(16)}
												className={styles.connectionDomainIcon}
												data-flx="user.user-profile-shared.user-profile-connections.connection-domain-icon"
											/>
										)}
									</a>
								</div>
							</Tooltip>
						);
					})}
				</div>
			</div>
		);
	}
	if (variant === 'cards' || variant === 'mobile') {
		const listClass = variant === 'cards' ? styles.connectionsGrid : styles.connectionsListMobile;
		const isMobile = variant === 'mobile';
		return (
			<div
				className={styles.connectionsContainer}
				data-flx="user.user-profile-shared.user-profile-connections.connections-container"
			>
				<span
					className={styles.connectionsTitle}
					data-flx="user.user-profile-shared.user-profile-connections.connections-title"
				>
					<Trans>Connections</Trans>
				</span>
				<div className={listClass} data-flx="user.user-profile-shared.user-profile-connections.div--2">
					{connections.map((connection) => (
						<ConnectionCard
							key={connection.id}
							connection={connection}
							onLinkClick={handleConnectionClick}
							mobile={isMobile}
							data-flx="user.user-profile-shared.user-profile-connections.connection-card"
						/>
					))}
				</div>
			</div>
		);
	}
	return null;
});
