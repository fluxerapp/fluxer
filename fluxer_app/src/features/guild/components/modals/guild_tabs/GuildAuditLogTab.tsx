// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmptySlate} from '@app/features/app/components/dialogs/shared/EmptySlate';
import {
	AUDIT_LOG_ACTIONS,
	AUDIT_LOG_TARGET_TYPES,
	type AuditLogTargetType,
	getTranslatedAuditLogActions,
} from '@app/features/app/config/AuditLogConstants';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTab.module.css';
import {
	type AuditLogActionKind,
	DEFAULT_FOR_STRINGS_KEY,
	LOG_PAGE_SIZE,
} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabConstants';
import {getRendererTableForTarget} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabRenderers';
import {
	type AuditLogUserOption,
	buildUserOptions,
	formatTimestamp,
	renderEntrySummary,
	renderFallbackChangeDetail,
	renderOptionDetailSentence,
	resolveChannelLabel,
	resolveTargetLabel,
	shouldNotRenderChangeDetail,
	shouldShowFallbackChangeDetail,
} from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTabUtils';
import {
	getActionKind,
	getTargetType,
	looksLikeSnowflake,
	normalizeChanges,
	resolveIdToName,
	safeScalarString,
	shouldSuppressDetailsForAction,
	toChangeShape,
} from '@app/features/guild/utils/guild_tabs/GuildAuditLogTabUtils';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import Users from '@app/features/user/state/Users';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import type {GuildAuditLogEntryResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type {IconWeight} from '@phosphor-icons/react';
import {
	BuildingsIcon,
	CaretDownIcon,
	ClipboardTextIcon,
	FunnelSimpleIcon,
	GearIcon,
	HashIcon,
	LinkIcon,
	MinusIcon,
	PencilSimpleIcon,
	PlugIcon,
	PlusIcon,
	SmileyIcon,
	StampIcon,
	TagIcon,
	UserGearIcon,
	WarningCircleIcon,
} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {ReactElement} from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const ALL_USERS_DESCRIPTOR = msg({
	message: 'All users',
	comment:
		'Default option in the activity log "Filter by user" dropdown. Means no user filter is applied. Short standalone label.',
});
const ALL_ACTIONS_DESCRIPTOR = msg({
	message: 'All actions',
	comment:
		'Default option in the activity log "Filter by action" dropdown. Means no action-type filter is applied. Short standalone label.',
});
const SOMETHING_WENT_WRONG_WHILE_LOADING_THE_ACTIVITY_LOG_DESCRIPTOR = msg({
	message: 'Something went wrong while loading the activity log.',
	comment: 'Generic error shown in the activity log tab when fetching log entries fails.',
});
const ACTIVITY_LOG_DESCRIPTOR = msg({
	message: 'Activity log',
	comment: 'Page title of the activity-log tab in community settings (the user-facing name for the audit log).',
});
const TRACK_MODERATOR_ACTIONS_ACROSS_THE_COMMUNITY_DESCRIPTOR = msg({
	message: 'Track moderator actions across the community.',
	comment: 'Subtitle under the activity-log page title.',
});
const FILTER_BY_USER_DESCRIPTOR = msg({
	message: 'Filter by user',
	comment: 'Label of the user filter dropdown in the activity log tab.',
});
const FILTER_BY_ACTION_DESCRIPTOR = msg({
	message: 'Filter by action',
	comment: 'Label of the action-type filter dropdown in the activity log tab.',
});
const NO_REASON_WAS_PROVIDED_DESCRIPTOR = msg({
	message: 'No reason was provided.',
	comment: 'Fallback text in an expanded activity log entry when the moderator did not supply a reason.',
});
const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback avatar label in the audit log when the acting user is unavailable.',
});
const NO_LOGS_YET_DESCRIPTOR = msg({
	message: 'No logs yet',
	comment: 'Empty-state title in the activity log tab when there are no log entries to show.',
});
const MODERATION_ACTIONS_AND_COMMUNITY_CHANGES_WILL_APPEAR_HERE_DESCRIPTOR = msg({
	message: 'Moderation actions and community changes will appear here.',
	comment: 'Empty-state body in the activity log tab when there are no log entries to show.',
});
const UNABLE_TO_LOAD_ACTIVITY_LOGS_DESCRIPTOR = msg({
	message: 'Unable to load activity logs',
	comment: 'Error-state title in the activity log tab when fetching log entries failed.',
});

type IconComponent = React.ComponentType<{size?: number | string; weight?: IconWeight; className?: string}>;
type ChangeTone = 'add' | 'remove' | 'update';

const logger = new Logger('GuildAuditLogTab');
const actionIconMap: Partial<Record<AuditLogActionType, IconComponent>> = {
	[AuditLogActionType.GUILD_UPDATE]: GearIcon,
	[AuditLogActionType.CHANNEL_CREATE]: HashIcon,
	[AuditLogActionType.CHANNEL_UPDATE]: HashIcon,
	[AuditLogActionType.CHANNEL_DELETE]: HashIcon,
	[AuditLogActionType.CHANNEL_OVERWRITE_CREATE]: HashIcon,
	[AuditLogActionType.CHANNEL_OVERWRITE_UPDATE]: HashIcon,
	[AuditLogActionType.CHANNEL_OVERWRITE_DELETE]: HashIcon,
	[AuditLogActionType.MEMBER_KICK]: UserGearIcon,
	[AuditLogActionType.MEMBER_PRUNE]: UserGearIcon,
	[AuditLogActionType.MEMBER_BAN_ADD]: UserGearIcon,
	[AuditLogActionType.MEMBER_BAN_REMOVE]: UserGearIcon,
	[AuditLogActionType.MEMBER_UPDATE]: UserGearIcon,
	[AuditLogActionType.MEMBER_ROLE_UPDATE]: UserGearIcon,
	[AuditLogActionType.MEMBER_MOVE]: UserGearIcon,
	[AuditLogActionType.MEMBER_DISCONNECT]: UserGearIcon,
	[AuditLogActionType.BOT_ADD]: UserGearIcon,
	[AuditLogActionType.ROLE_CREATE]: TagIcon,
	[AuditLogActionType.ROLE_UPDATE]: TagIcon,
	[AuditLogActionType.ROLE_DELETE]: TagIcon,
	[AuditLogActionType.INVITE_CREATE]: LinkIcon,
	[AuditLogActionType.INVITE_UPDATE]: LinkIcon,
	[AuditLogActionType.INVITE_DELETE]: LinkIcon,
	[AuditLogActionType.WEBHOOK_CREATE]: PlugIcon,
	[AuditLogActionType.WEBHOOK_UPDATE]: PlugIcon,
	[AuditLogActionType.WEBHOOK_DELETE]: PlugIcon,
	[AuditLogActionType.EMOJI_CREATE]: SmileyIcon,
	[AuditLogActionType.EMOJI_UPDATE]: SmileyIcon,
	[AuditLogActionType.EMOJI_DELETE]: SmileyIcon,
	[AuditLogActionType.STICKER_CREATE]: StampIcon,
	[AuditLogActionType.STICKER_UPDATE]: StampIcon,
	[AuditLogActionType.STICKER_DELETE]: StampIcon,
	[AuditLogActionType.MESSAGE_DELETE]: PencilSimpleIcon,
	[AuditLogActionType.MESSAGE_BULK_DELETE]: PencilSimpleIcon,
	[AuditLogActionType.MESSAGE_PIN]: PencilSimpleIcon,
	[AuditLogActionType.MESSAGE_UNPIN]: PencilSimpleIcon,
};
const targetIconMap: Record<AuditLogTargetType, IconComponent> = {
	[AUDIT_LOG_TARGET_TYPES.ALL]: BuildingsIcon,
	[AUDIT_LOG_TARGET_TYPES.GUILD]: GearIcon,
	[AUDIT_LOG_TARGET_TYPES.MEMBER]: UserGearIcon,
	[AUDIT_LOG_TARGET_TYPES.CHANNEL]: HashIcon,
	[AUDIT_LOG_TARGET_TYPES.USER]: UserGearIcon,
	[AUDIT_LOG_TARGET_TYPES.ROLE]: TagIcon,
	[AUDIT_LOG_TARGET_TYPES.INVITE]: LinkIcon,
	[AUDIT_LOG_TARGET_TYPES.WEBHOOK]: PlugIcon,
	[AUDIT_LOG_TARGET_TYPES.EMOJI]: SmileyIcon,
	[AUDIT_LOG_TARGET_TYPES.STICKER]: StampIcon,
	[AUDIT_LOG_TARGET_TYPES.MESSAGE]: PencilSimpleIcon,
};
const getActionIcon = (actionType: AuditLogActionType): IconComponent => {
	const targetType = getTargetType(actionType);
	return targetIconMap[targetType as AuditLogTargetType] ?? actionIconMap[actionType] ?? BuildingsIcon;
};
const getActionOptionIcon = (value: string): IconComponent => {
	if (!value) return FunnelSimpleIcon;
	const action = AUDIT_LOG_ACTIONS.find((item) => item.value.toString() === value);
	if (!action) return FunnelSimpleIcon;
	const actionType = Number(value) as AuditLogActionType;
	const targetType = getTargetType(actionType);
	return targetIconMap[targetType as AuditLogTargetType] ?? getActionIcon(actionType);
};
const USER_FILTER_AVATAR_SIZE = 28;
const getActionSelectIconToneClass = (actionKind: AuditLogActionKind): string => {
	switch (actionKind) {
		case 'create':
			return styles.actionSelectIconCreate;
		case 'update':
			return styles.actionSelectIconUpdate;
		case 'delete':
			return styles.actionSelectIconDelete;
		default:
			return styles.actionSelectIconNeutral;
	}
};
const getChangeTone = (change: {key: string; oldValue: unknown; newValue: unknown}): ChangeTone => {
	if (change.key === '$remove') return 'remove';
	if (typeof change.newValue === 'boolean' && typeof change.oldValue === 'boolean') {
		return change.newValue ? 'add' : 'remove';
	}
	if (change.oldValue != null && change.newValue == null) return 'remove';
	return 'add';
};
const getOptionTone = (value: unknown): ChangeTone => {
	if (typeof value === 'boolean') return value ? 'add' : 'remove';
	return 'add';
};
const getChangeIcon = (tone: ChangeTone): IconComponent => {
	switch (tone) {
		case 'remove':
			return MinusIcon;
		default:
			return PlusIcon;
	}
};
const maybeUrlDecodeReason = (raw: string): string => {
	if (!/%[0-9A-Fa-f]{2}/.test(raw)) return raw;
	try {
		const decoded = decodeURIComponent(raw);
		return decoded === raw ? raw : decoded;
	} catch {
		return raw;
	}
};
const INFINITE_SCROLL_OVERSCAN_PX = 1200;
const getChangeBulletToneClass = (tone: ChangeTone): string => {
	if (tone === 'remove') {
		return styles.changeBulletRemove;
	}
	return styles.changeBulletAdd;
};

type UserFilterOption = AuditLogUserOption | ComboboxOption<string>;

const isAuditLogUserOption = (option: UserFilterOption): option is AuditLogUserOption =>
	'user' in option && option.user != null;
const GuildAuditLogTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const [entries, setEntries] = useState<Array<GuildAuditLogEntryResponse>>([]);
	const [isLoading, setIsLoading] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const [hasSuccessfulEmptyLoad, setHasSuccessfulEmptyLoad] = useState(false);
	const [selectedUserId, setSelectedUserId] = useState('');
	const [selectedAction, setSelectedAction] = useState('');
	const [hasMore, setHasMore] = useState(true);
	const [expandedEntryId, setExpandedEntryId] = useState<string | null>(null);
	const members = GuildMembers.getMembers(guildId);
	const userOptions = useMemo<Array<UserFilterOption>>(
		() => [{value: '', label: i18n._(ALL_USERS_DESCRIPTOR)}, ...buildUserOptions(members)],
		[members, i18n.locale],
	);
	const actionOptions = useMemo<Array<ComboboxOption<string>>>(
		() => [
			{value: '', label: i18n._(ALL_ACTIONS_DESCRIPTOR)},
			...getTranslatedAuditLogActions(i18n).map((action) => ({
				value: action.value.toString(),
				label: action.label,
			})),
		],
		[i18n.locale],
	);
	const userComboboxRenderers = useMemo(() => {
		const renderContent = (option: UserFilterOption) => {
			if (!option.value) {
				return (
					<div
						className={styles.userSelectRowGlobal}
						data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.user-select-row-global"
					>
						<span
							className={styles.userSelectLabel}
							data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.user-select-label"
						>
							{option.label}
						</span>
					</div>
				);
			}
			return (
				<div
					className={styles.userSelectRow}
					data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.user-select-row"
				>
					{isAuditLogUserOption(option) && (
						<div
							className={styles.userSelectAvatarWrapper}
							data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.user-select-avatar-wrapper"
						>
							<Avatar
								user={option.user}
								size={USER_FILTER_AVATAR_SIZE}
								guildId={guildId}
								data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.avatar"
							/>
						</div>
					)}
					<span
						className={styles.userSelectLabel}
						data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.user-select-label--2"
					>
						{option.label}
					</span>
				</div>
			);
		};
		return {
			renderOption: renderContent,
			renderValue: (option: UserFilterOption | null) => (option ? renderContent(option) : null),
		};
	}, [guildId]);
	const actionComboboxRenderers = useMemo(() => {
		const renderContent = (option: ComboboxOption<string>) => {
			const Icon = getActionOptionIcon(option.value);
			const actionKind = option.value ? getActionKind(Number(option.value) as AuditLogActionType) : null;
			const actionToneClass =
				actionKind != null ? getActionSelectIconToneClass(actionKind) : styles.actionSelectIconNeutral;
			return (
				<div
					className={styles.actionSelectRow}
					data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.action-select-row"
				>
					<span
						className={clsx(styles.actionSelectIcon, actionToneClass)}
						aria-hidden
						data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.action-select-icon"
					>
						<Icon
							size={remFromPx(18)}
							weight="bold"
							data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.icon"
						/>
					</span>
					<span
						className={styles.actionSelectLabel}
						data-flx="guild.guild-tabs.guild-audit-log-tab.render-content.action-select-label"
					>
						{option.label}
					</span>
				</div>
			);
		};
		return {
			renderOption: renderContent,
			renderValue: (option: ComboboxOption<string> | null) => (option ? renderContent(option) : null),
		};
	}, []);
	const loadLogs = useCallback(
		async ({reset = false, before}: {reset?: boolean; before?: string | null} = {}) => {
			setIsLoading(true);
			setError(null);
			try {
				const actionType = selectedAction ? Number(selectedAction) : undefined;
				const response = await GuildCommands.fetchGuildAuditLogs(guildId, {
					limit: LOG_PAGE_SIZE,
					beforeLogId: reset ? undefined : (before ?? undefined),
					userId: selectedUserId || undefined,
					actionType: actionType ?? undefined,
				});
				const fetchedEntries = response.audit_log_entries;
				Users.cacheUsers(response.users);
				setEntries((current) => {
					const updatedEntries = reset ? fetchedEntries : [...current, ...fetchedEntries];
					setHasSuccessfulEmptyLoad(reset && updatedEntries.length === 0);
					return updatedEntries;
				});
				setHasMore(fetchedEntries.length === LOG_PAGE_SIZE);
				if (reset) setExpandedEntryId(null);
			} catch (err) {
				logger.error('Failed to load audit logs', err);
				setError(i18n._(SOMETHING_WENT_WRONG_WHILE_LOADING_THE_ACTIVITY_LOG_DESCRIPTOR));
				setHasSuccessfulEmptyLoad(false);
			} finally {
				setIsLoading(false);
			}
		},
		[guildId, selectedAction, selectedUserId],
	);
	useEffect(() => {
		loadLogs({reset: true});
	}, [loadLogs]);
	useEffect(() => {
		if (entries.length === 0) {
			return;
		}
		const userIds = new Set<string>();
		for (const entry of entries) {
			if (entry.user_id) {
				userIds.add(entry.user_id);
			}
			if (entry.target_id) {
				userIds.add(entry.target_id);
			}
		}
		if (userIds.size > 0) {
			GuildMembers.ensureMembersLoaded(guildId, Array.from(userIds)).catch((error) => {
				logger.error('Failed to ensure members', error);
			});
		}
	}, [guildId, entries]);
	const shouldShowErrorState = Boolean(error);
	const errorDescription = error ?? i18n._(SOMETHING_WENT_WRONG_WHILE_LOADING_THE_ACTIVITY_LOG_DESCRIPTOR);
	const shouldShowEmptyState = !shouldShowErrorState && !isLoading && entries.length === 0 && hasSuccessfulEmptyLoad;
	const handleLoadMore = useCallback(() => {
		if (!hasMore || entries.length === 0 || isLoading) return;
		loadLogs({before: entries[entries.length - 1].id});
	}, [entries, hasMore, isLoading, loadLogs]);
	const sentinelRef = useRef<HTMLDivElement | null>(null);
	useEffect(() => {
		const el = sentinelRef.current;
		if (!el) return;
		if (!hasMore || entries.length === 0 || error != null) return;
		const observer = new IntersectionObserver(
			(observed) => {
				if (observed.some((e) => e.isIntersecting)) {
					handleLoadMore();
				}
			},
			{rootMargin: `0px 0px ${INFINITE_SCROLL_OVERSCAN_PX}px 0px`, threshold: 0},
		);
		observer.observe(el);
		return () => observer.disconnect();
	}, [handleLoadMore, hasMore, entries.length, error]);
	const toggleExpanded = (id: string) => setExpandedEntryId((current) => (current === id ? null : id));
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-audit-log-tab.container">
			<div className={styles.headerTop} data-flx="guild.guild-tabs.guild-audit-log-tab.header-top">
				<h2 className={styles.pageTitle} data-flx="guild.guild-tabs.guild-audit-log-tab.page-title">
					{i18n._(ACTIVITY_LOG_DESCRIPTOR)}
				</h2>
				<p className={styles.pageSubtitle} data-flx="guild.guild-tabs.guild-audit-log-tab.page-subtitle">
					{i18n._(TRACK_MODERATOR_ACTIONS_ACROSS_THE_COMMUNITY_DESCRIPTOR)}
				</p>
			</div>
			<div className={styles.filterRow} data-flx="guild.guild-tabs.guild-audit-log-tab.filter-row">
				<Combobox
					value={selectedUserId}
					onChange={(value) => setSelectedUserId(value)}
					options={userOptions}
					placeholder={i18n._(ALL_USERS_DESCRIPTOR)}
					renderOption={userComboboxRenderers.renderOption}
					renderValue={userComboboxRenderers.renderValue}
					label={i18n._(FILTER_BY_USER_DESCRIPTOR)}
					data-flx="guild.guild-tabs.guild-audit-log-tab.select"
				/>
				<Combobox
					value={selectedAction}
					onChange={(value) => setSelectedAction(value)}
					options={actionOptions}
					placeholder={i18n._(ALL_ACTIONS_DESCRIPTOR)}
					label={i18n._(FILTER_BY_ACTION_DESCRIPTOR)}
					renderOption={actionComboboxRenderers.renderOption}
					renderValue={actionComboboxRenderers.renderValue}
					data-flx="guild.guild-tabs.guild-audit-log-tab.select--2"
				/>
			</div>
			<div className={styles.entries} data-flx="guild.guild-tabs.guild-audit-log-tab.entries">
				{isLoading && entries.length === 0 && (
					<div className={styles.spinnerRow} data-flx="guild.guild-tabs.guild-audit-log-tab.spinner-row">
						<Spinner size="large" data-flx="guild.guild-tabs.guild-audit-log-tab.spinner" />
					</div>
				)}
				{entries.length > 0 && !shouldShowErrorState && (
					<div className={styles.entryList} data-flx="guild.guild-tabs.guild-audit-log-tab.entry-list">
						{entries.map((entry) => {
							const entryId = entry.id;
							const targetType = getTargetType(entry.action_type as AuditLogActionType);
							const actionKind = getActionKind(entry.action_type as AuditLogActionType);
							const targetClassKey = `target_${targetType}` as keyof typeof styles;
							const actionClassKey = `type_${actionKind}` as keyof typeof styles;
							const entryClasses = clsx(styles.auditLog, styles[targetClassKey], styles[actionClassKey]);
							const ActionIcon = getActionIcon(entry.action_type as AuditLogActionType);
							const actorUser = entry.user_id ? (Users.getUser(entry.user_id) ?? null) : null;
							const targetUser = entry.target_id ? (Users.getUser(entry.target_id) ?? null) : null;
							const targetLabel = resolveTargetLabel(entry, i18n);
							const channelLabel = resolveChannelLabel(entry, guildId, i18n);
							const summaryNode = renderEntrySummary({
								entry,
								actorUser,
								targetUser,
								targetLabel,
								channelLabel,
								guildId,
								i18n,
							});
							const suppressDetails = shouldSuppressDetailsForAction(entry.action_type as AuditLogActionType);
							const changeShapes = suppressDetails
								? []
								: normalizeChanges(entry.changes)
										.map(toChangeShape)
										.filter((change) => change.key && !shouldNotRenderChangeDetail(targetType, change.key));
							const rendererTable = getRendererTableForTarget(targetType);
							const renderedChangeKeys = new Set(
								changeShapes
									.filter((change) => rendererTable[change.key]?.(change, {entry, guildId, i18n}) != null)
									.map((change) => change.key),
							);
							const optionEntries =
								suppressDetails || !entry.options
									? []
									: Object.entries(entry.options).filter(([key, value]) => {
											if (key === DEFAULT_FOR_STRINGS_KEY) return false;
											if (renderedChangeKeys.has(key)) return false;
											if (
												key !== 'channel_id' &&
												key !== 'message_id' &&
												key !== 'inviter_id' &&
												(key === 'id' || key.endsWith('_id'))
											)
												return false;
											const scalar = safeScalarString(value, i18n);
											if (scalar && looksLikeSnowflake(scalar)) {
												return resolveIdToName(scalar, guildId) != null;
											}
											return true;
										});
							const rawReason = typeof entry.reason === 'string' && entry.reason.trim() ? entry.reason.trim() : '';
							const decodedReason = rawReason ? maybeUrlDecodeReason(rawReason) : '';
							const reasonText = decodedReason || i18n._(NO_REASON_WAS_PROVIDED_DESCRIPTOR);
							const changeRows = changeShapes
								.map((change, changeIndex) => {
									const renderer = rendererTable[change.key];
									const rendered = renderer?.(change, {entry, guildId, i18n});
									if (!rendered && !shouldShowFallbackChangeDetail(change)) {
										return null;
									}
									const tone = getChangeTone(change);
									const ChangeIcon = getChangeIcon(tone);
									const toneClass = getChangeBulletToneClass(tone);
									return (
										<div
											className={styles.changeItem}
											key={`${entryId}-${change.key}-${changeIndex}`}
											data-flx="guild.guild-tabs.guild-audit-log-tab.change-item"
										>
											<span
												className={clsx(styles.changeBullet, toneClass)}
												aria-hidden
												data-flx="guild.guild-tabs.guild-audit-log-tab.change-bullet"
											>
												<ChangeIcon
													size={remFromPx(12)}
													weight="bold"
													className={styles.changeBulletIcon}
													data-flx="guild.guild-tabs.guild-audit-log-tab.change-bullet-icon"
												/>
											</span>
											<span className={styles.changeText} data-flx="guild.guild-tabs.guild-audit-log-tab.change-text">
												{rendered ?? renderFallbackChangeDetail(change, guildId, i18n)}
											</span>
										</div>
									);
								})
								.filter((row): row is ReactElement => row !== null);
							const shouldShowReasonPreview = typeof entry.reason === 'string' && entry.reason.trim().length > 0;
							const isExpandable = shouldShowReasonPreview || changeRows.length > 0 || optionEntries.length > 0;
							const isExpandedView = isExpandable && expandedEntryId === entryId;
							const headerClasses = clsx(styles.header, {
								[styles.headerClickable]: isExpandable,
								[styles.headerStatic]: !isExpandable,
								[styles.headerExpanded]: isExpandedView,
								[styles.headerDefault]: !isExpandedView,
							});
							return (
								<div key={entryId} className={entryClasses} data-flx="guild.guild-tabs.guild-audit-log-tab.div">
									{isExpandable ? (
										<FocusRing offset={-2} data-flx="guild.guild-tabs.guild-audit-log-tab.focus-ring">
											<button
												type="button"
												onClick={() => toggleExpanded(entryId)}
												className={headerClasses}
												aria-expanded={isExpandedView}
												data-flx="guild.guild-tabs.guild-audit-log-tab.button.toggle-expanded"
											>
												<span className={styles.icon} aria-hidden data-flx="guild.guild-tabs.guild-audit-log-tab.icon">
													<ActionIcon
														size={remFromPx(20)}
														weight="bold"
														className={styles.iconGlyph}
														data-flx="guild.guild-tabs.guild-audit-log-tab.icon-glyph"
													/>
												</span>
												<div className={styles.avatar} data-flx="guild.guild-tabs.guild-audit-log-tab.avatar">
													{actorUser ? (
														<Avatar
															user={actorUser}
															size={32}
															guildId={guildId}
															data-flx="guild.guild-tabs.guild-audit-log-tab.avatar--2"
														/>
													) : (
														<MockAvatar
															size={32}
															userTag={entry.user_id ?? i18n._(UNKNOWN_USER_DESCRIPTOR)}
															data-flx="guild.guild-tabs.guild-audit-log-tab.mock-avatar"
														/>
													)}
												</div>
												<div className={styles.textBlock} data-flx="guild.guild-tabs.guild-audit-log-tab.text-block">
													<div className={styles.titleRow} data-flx="guild.guild-tabs.guild-audit-log-tab.title-row">
														<span className={styles.summary} data-flx="guild.guild-tabs.guild-audit-log-tab.summary">
															{summaryNode}
														</span>
													</div>
													<div className={styles.metaRow} data-flx="guild.guild-tabs.guild-audit-log-tab.meta-row">
														<span
															className={styles.timestamp}
															data-flx="guild.guild-tabs.guild-audit-log-tab.timestamp"
														>
															{formatTimestamp(entry.id)}
														</span>
													</div>
												</div>
												<CaretDownIcon
													size={remFromPx(20)}
													weight="bold"
													className={clsx(styles.chevron, {[styles.chevronExpanded]: isExpandedView})}
													data-flx="guild.guild-tabs.guild-audit-log-tab.chevron"
												/>
											</button>
										</FocusRing>
									) : (
										<div className={headerClasses} data-flx="guild.guild-tabs.guild-audit-log-tab.div--2">
											<span className={styles.icon} aria-hidden data-flx="guild.guild-tabs.guild-audit-log-tab.icon--2">
												<ActionIcon
													size={remFromPx(20)}
													weight="bold"
													className={styles.iconGlyph}
													data-flx="guild.guild-tabs.guild-audit-log-tab.icon-glyph--2"
												/>
											</span>
											<div className={styles.avatar} data-flx="guild.guild-tabs.guild-audit-log-tab.avatar--3">
												{actorUser ? (
													<Avatar
														user={actorUser}
														size={32}
														guildId={guildId}
														data-flx="guild.guild-tabs.guild-audit-log-tab.avatar--4"
													/>
												) : (
													<MockAvatar
														size={32}
														userTag={entry.user_id ?? i18n._(UNKNOWN_USER_DESCRIPTOR)}
														data-flx="guild.guild-tabs.guild-audit-log-tab.mock-avatar--2"
													/>
												)}
											</div>
											<div className={styles.textBlock} data-flx="guild.guild-tabs.guild-audit-log-tab.text-block--2">
												<div className={styles.titleRow} data-flx="guild.guild-tabs.guild-audit-log-tab.title-row--2">
													<span className={styles.summary} data-flx="guild.guild-tabs.guild-audit-log-tab.summary--2">
														{summaryNode}
													</span>
												</div>
												<div className={styles.metaRow} data-flx="guild.guild-tabs.guild-audit-log-tab.meta-row--2">
													<span
														className={styles.timestamp}
														data-flx="guild.guild-tabs.guild-audit-log-tab.timestamp--2"
													>
														{formatTimestamp(entry.id)}
													</span>
												</div>
											</div>
										</div>
									)}
									{isExpandedView && (
										<div className={styles.details} data-flx="guild.guild-tabs.guild-audit-log-tab.details">
											{shouldShowReasonPreview && (
												<div className={styles.reasonRow} data-flx="guild.guild-tabs.guild-audit-log-tab.reason-row">
													<span
														className={styles.reasonLabel}
														data-flx="guild.guild-tabs.guild-audit-log-tab.reason-label"
													>
														<Trans>Reason</Trans>
													</span>
													<span
														className={styles.reasonValue}
														data-flx="guild.guild-tabs.guild-audit-log-tab.reason-value"
													>
														{reasonText}
													</span>
												</div>
											)}
											{(changeRows.length > 0 || optionEntries.length > 0) && (
												<div className={styles.changeList} data-flx="guild.guild-tabs.guild-audit-log-tab.change-list">
													{changeRows}
													{optionEntries.map(([key, value]) => (
														<div
															className={styles.changeItem}
															key={key}
															data-flx="guild.guild-tabs.guild-audit-log-tab.change-item--2"
														>
															{(() => {
																const tone = getOptionTone(value);
																const ChangeIcon = getChangeIcon(tone);
																const toneClass = getChangeBulletToneClass(tone);
																return (
																	<span
																		className={clsx(styles.changeBullet, toneClass)}
																		aria-hidden
																		data-flx="guild.guild-tabs.guild-audit-log-tab.change-bullet--2"
																	>
																		<ChangeIcon
																			size={remFromPx(12)}
																			weight="bold"
																			className={styles.changeBulletIcon}
																			data-flx="guild.guild-tabs.guild-audit-log-tab.change-bullet-icon--2"
																		/>
																	</span>
																);
															})()}
															<span
																className={styles.changeText}
																data-flx="guild.guild-tabs.guild-audit-log-tab.change-text--2"
															>
																{renderOptionDetailSentence(
																	key,
																	value,
																	guildId,
																	entry.action_type as AuditLogActionType,
																	i18n,
																)}
															</span>
														</div>
													))}
												</div>
											)}
										</div>
									)}
								</div>
							);
						})}
					</div>
				)}
				{shouldShowEmptyState && (
					<div className={styles.emptyState} data-flx="guild.guild-tabs.guild-audit-log-tab.empty-state">
						<EmptySlate
							Icon={ClipboardTextIcon}
							title={i18n._(NO_LOGS_YET_DESCRIPTOR)}
							description={i18n._(MODERATION_ACTIONS_AND_COMMUNITY_CHANGES_WILL_APPEAR_HERE_DESCRIPTOR)}
							data-flx="guild.guild-tabs.guild-audit-log-tab.empty-slate"
						/>
					</div>
				)}
				{!isLoading && shouldShowErrorState && (
					<div className={styles.errorState} data-flx="guild.guild-tabs.guild-audit-log-tab.error-state">
						<EmptySlate
							Icon={WarningCircleIcon}
							title={i18n._(UNABLE_TO_LOAD_ACTIVITY_LOGS_DESCRIPTOR)}
							description={errorDescription}
							data-flx="guild.guild-tabs.guild-audit-log-tab.empty-slate--2"
						/>
						<div className={styles.statusActions} data-flx="guild.guild-tabs.guild-audit-log-tab.status-actions">
							<Button
								variant="secondary"
								onClick={() => loadLogs({reset: true})}
								data-flx="guild.guild-tabs.guild-audit-log-tab.button.load-logs"
							>
								{i18n._(TRY_AGAIN_DESCRIPTOR)}
							</Button>
						</div>
					</div>
				)}
				{hasMore && entries.length > 0 && !shouldShowErrorState && (
					<div className={styles.loadMore} data-flx="guild.guild-tabs.guild-audit-log-tab.load-more">
						<div
							ref={sentinelRef}
							aria-hidden
							style={{height: 1, width: '100%'}}
							data-flx="guild.guild-tabs.guild-audit-log-tab.div--3"
						/>
						{isLoading && <Spinner size="medium" data-flx="guild.guild-tabs.guild-audit-log-tab.spinner--2" />}
					</div>
				)}
			</div>
		</div>
	);
});

export default GuildAuditLogTab;
