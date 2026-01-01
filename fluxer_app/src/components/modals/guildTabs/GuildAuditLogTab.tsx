/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

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
import {useCallback, useEffect, useMemo, useState} from 'react';
import {type OptionProps, components as reactSelectComponents, type SingleValueProps} from 'react-select';
import type {GuildAuditLogEntry} from '~/actions/GuildActionCreators';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import {Select, type SelectOption} from '~/components/form/Select';
import {EmptySlate} from '~/components/modals/shared/EmptySlate';
import {Avatar} from '~/components/uikit/Avatar';
import {Button} from '~/components/uikit/Button/Button';
import {MockAvatar} from '~/components/uikit/MockAvatar';
import {Spinner} from '~/components/uikit/Spinner';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {AUDIT_LOG_ACTIONS, getTranslatedAuditLogActions} from '~/constants/AuditLogConstants';
import {Logger} from '~/lib/Logger';
import GuildMemberStore from '~/stores/GuildMemberStore';
import UserStore from '~/stores/UserStore';
import {
	getActionKind,
	getTargetType,
	looksLikeSnowflake,
	normalizeChanges,
	resolveIdToName,
	safeScalarString,
	shouldSuppressDetailsForAction,
	toChangeShape,
} from '~/utils/modals/guildTabs/GuildAuditLogTabUtils';
import {AuditLogTargetType, DEFAULT_FOR_STRINGS_KEY, LOG_PAGE_SIZE} from './GuildAuditLogTab.constants';
import styles from './GuildAuditLogTab.module.css';
import {getRendererTableForTarget} from './GuildAuditLogTab.renderers';
import type {AuditLogUserOption} from './GuildAuditLogTab.utils';
import {
	buildUserOptions,
	formatTimestamp,
	renderEntrySummary,
	renderFallbackChangeDetail,
	renderOptionDetailSentence,
	resolveChannelLabel,
	resolveTargetLabel,
	shouldNotRenderChangeDetail,
	shouldShowFallbackChangeDetail,
} from './GuildAuditLogTab.utils';

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
	[AuditLogTargetType.ALL]: BuildingsIcon,
	[AuditLogTargetType.GUILD]: GearIcon,
	[AuditLogTargetType.CHANNEL]: HashIcon,
	[AuditLogTargetType.USER]: UserGearIcon,
	[AuditLogTargetType.ROLE]: TagIcon,
	[AuditLogTargetType.INVITE]: LinkIcon,
	[AuditLogTargetType.WEBHOOK]: PlugIcon,
	[AuditLogTargetType.EMOJI]: SmileyIcon,
	[AuditLogTargetType.STICKER]: StampIcon,
	[AuditLogTargetType.MESSAGE]: PencilSimpleIcon,
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

type UserFilterOption = AuditLogUserOption | SelectOption<string>;
const isAuditLogUserOption = (option: UserFilterOption): option is AuditLogUserOption =>
	'user' in option && option.user != null;

const GuildAuditLogTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {t, i18n} = useLingui();
	const translate = t ?? i18n._;
	const [entries, setEntries] = useState<Array<GuildAuditLogEntry>>([]);
	const [isLoading, setIsLoading] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const [hasSuccessfulEmptyLoad, setHasSuccessfulEmptyLoad] = useState(false);
	const [selectedUserId, setSelectedUserId] = useState('');
	const [selectedAction, setSelectedAction] = useState('');
	const [hasMore, setHasMore] = useState(true);
	const [expandedEntryId, setExpandedEntryId] = useState<string | null>(null);

	const members = GuildMemberStore.getMembers(guildId);
	const userOptions = useMemo<Array<UserFilterOption>>(
		() => [{value: '', label: t`All Users`}, ...buildUserOptions(members)],
		[members],
	);

	const actionOptions = useMemo<Array<SelectOption<string>>>(
		() => [
			{value: '', label: t`All Actions`},
			...getTranslatedAuditLogActions(i18n).map((action) => ({
				value: action.value.toString(),
				label: action.label,
			})),
		],
		[],
	);

	const userSelectComponents = useMemo(() => {
		const renderContent = (option: UserFilterOption) => {
			if (!option.value) {
				return (
					<div className={styles.userSelectRowGlobal}>
						<span className={styles.userSelectLabel}>{option.label}</span>
					</div>
				);
			}

			return (
				<div className={styles.userSelectRow}>
					{isAuditLogUserOption(option) && (
						<div className={styles.userSelectAvatarWrapper}>
							<Avatar user={option.user} size={USER_FILTER_AVATAR_SIZE} guildId={guildId} />
						</div>
					)}
					<span className={styles.userSelectLabel}>{option.label}</span>
				</div>
			);
		};

		const OptionComponent = observer((props: OptionProps<UserFilterOption, false>) => (
			<reactSelectComponents.Option {...props}>{renderContent(props.data)}</reactSelectComponents.Option>
		));

		const SingleValueComponent = observer((props: SingleValueProps<UserFilterOption, false>) => (
			<reactSelectComponents.SingleValue {...props}>{renderContent(props.data)}</reactSelectComponents.SingleValue>
		));

		return {Option: OptionComponent, SingleValue: SingleValueComponent};
	}, [guildId]);

	const actionSelectComponents = useMemo(() => {
		const renderContent = (option: SelectOption<string>) => {
			const Icon = getActionOptionIcon(option.value);
			const actionKind = option.value ? getActionKind(Number(option.value) as AuditLogActionType) : null;
			const actionToneClass =
				actionKind != null
					? styles[`actionSelectIcon_${actionKind}` as keyof typeof styles]
					: styles.actionSelectIconNeutral;

			return (
				<div className={styles.actionSelectRow}>
					<span className={clsx(styles.actionSelectIcon, actionToneClass)} aria-hidden>
						<Icon size={18} weight="bold" />
					</span>
					<span className={styles.actionSelectLabel}>{option.label}</span>
				</div>
			);
		};

		const OptionComponent = observer((props: OptionProps<SelectOption<string>, false>) => (
			<reactSelectComponents.Option {...props}>{renderContent(props.data)}</reactSelectComponents.Option>
		));

		const SingleValueComponent = observer((props: SingleValueProps<SelectOption<string>, false>) => (
			<reactSelectComponents.SingleValue {...props}>{renderContent(props.data)}</reactSelectComponents.SingleValue>
		));

		return {Option: OptionComponent, SingleValue: SingleValueComponent};
	}, []);

	const loadLogs = useCallback(
		async ({reset = false, before}: {reset?: boolean; before?: string | null} = {}) => {
			setIsLoading(true);
			setError(null);

			try {
				const actionType = selectedAction ? Number(selectedAction) : undefined;

				const response = await GuildActionCreators.fetchGuildAuditLogs(guildId, {
					limit: LOG_PAGE_SIZE,
					beforeLogId: reset ? undefined : (before ?? undefined),
					userId: selectedUserId || undefined,
					actionType: actionType ?? undefined,
				});

				const fetchedEntries = response.audit_log_entries;
				UserStore.cacheUsers(response.users);

				setEntries((current) => {
					const updatedEntries = reset ? fetchedEntries : [...current, ...fetchedEntries];
					setHasSuccessfulEmptyLoad(reset && updatedEntries.length === 0);
					return updatedEntries;
				});

				setHasMore(fetchedEntries.length === LOG_PAGE_SIZE);

				if (reset) setExpandedEntryId(null);
			} catch (err) {
				logger.error('Failed to load audit logs', err);
				setError(t`Something went wrong while loading the audit log.`);
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
			GuildMemberStore.ensureMembersLoaded(guildId, Array.from(userIds));
		}
	}, [guildId, entries]);

	const shouldShowErrorState = Boolean(error);
	const errorDescription = error ?? t`Something went wrong while loading the audit log.`;

	const shouldShowEmptyState = !shouldShowErrorState && !isLoading && entries.length === 0 && hasSuccessfulEmptyLoad;

	const handleLoadMore = useCallback(() => {
		if (!hasMore || entries.length === 0) return;
		loadLogs({before: entries[entries.length - 1].id});
	}, [entries, hasMore, loadLogs]);

	const toggleExpanded = (id: string) => setExpandedEntryId((current) => (current === id ? null : id));

	return (
		<div className={styles.container}>
			<div className={styles.headerTop}>
				<h2 className={styles.pageTitle}>{t`Activity Log`}</h2>
				<p className={styles.pageSubtitle}>{t`Track moderator actions across the community.`}</p>
			</div>

			<div className={styles.filterRow}>
				<Select
					value={selectedUserId}
					onChange={(value) => setSelectedUserId(value)}
					options={userOptions}
					placeholder={t`All Users`}
					components={userSelectComponents}
					label={t`Filter by user`}
				/>
				<Select
					value={selectedAction}
					onChange={(value) => setSelectedAction(value)}
					options={actionOptions}
					placeholder={t`All Actions`}
					label={t`Filter by action`}
					components={actionSelectComponents}
				/>
			</div>

			<div className={styles.entries}>
				{isLoading && entries.length === 0 && (
					<div className={styles.spinnerRow}>
						<Spinner size="large" />
					</div>
				)}

				{entries.length > 0 && !shouldShowErrorState && (
					<div className={styles.entryList}>
						{entries.map((entry) => {
							const entryId = entry.id;

							const targetType = getTargetType(entry.action_type as AuditLogActionType);
							const actionKind = getActionKind(entry.action_type as AuditLogActionType);

							const targetClassKey = `target_${targetType}` as keyof typeof styles;
							const actionClassKey = `type_${actionKind}` as keyof typeof styles;

							const entryClasses = clsx(styles.auditLog, styles[targetClassKey], styles[actionClassKey]);

							const ActionIcon = getActionIcon(entry.action_type as AuditLogActionType);

							const actorUser = entry.user_id ? (UserStore.getUser(entry.user_id) ?? null) : null;
							const targetUser = entry.target_id ? (UserStore.getUser(entry.target_id) ?? null) : null;

							const targetLabel = resolveTargetLabel(entry, translate);
							const channelLabel = resolveChannelLabel(entry, guildId, translate);

							const summaryNode = renderEntrySummary({
								entry,
								actorUser,
								targetUser,
								targetLabel,
								channelLabel,
								guildId,
								t: translate,
							});

							const suppressDetails = shouldSuppressDetailsForAction(entry.action_type as AuditLogActionType);
							const changeShapes = suppressDetails
								? []
								: normalizeChanges(entry.changes)
										.map(toChangeShape)
										.filter((change) => change.key && !shouldNotRenderChangeDetail(targetType, change.key));

							const rendererTable = getRendererTableForTarget(targetType);

							const actionType = entry.action_type as AuditLogActionType;
							const renderedChangeKeys = new Set(
								changeShapes
									.filter((change) => rendererTable[change.key]?.(change, {entry, guildId, t: translate}) != null)
									.map((change) => change.key),
							);
							const optionEntries = entry.options
								? Object.entries(entry.options).filter(([key, value]) => {
										if (key === DEFAULT_FOR_STRINGS_KEY) return false;
										if (renderedChangeKeys.has(key)) return false;
										if (actionType === AuditLogActionType.MESSAGE_BULK_DELETE) {
											if (
												key === 'count' ||
												key === 'message_count' ||
												key === 'delete_count' ||
												key === 'channel_id'
											) {
												return false;
											}
										}
										if (
											key !== 'channel_id' &&
											key !== 'message_id' &&
											key !== 'inviter_id' &&
											(key === 'id' || key.endsWith('_id'))
										)
											return false;
										const scalar = safeScalarString(value, translate);
										if (scalar && looksLikeSnowflake(scalar)) {
											return resolveIdToName(scalar, guildId) != null;
										}
										return true;
									})
								: [];

							const reasonText =
								typeof entry.reason === 'string' && entry.reason.trim()
									? entry.reason.trim()
									: t`No reason was provided.`;

							const changeRows = changeShapes
								.map((change, changeIndex) => {
									const renderer = rendererTable[change.key];
									const rendered = renderer?.(change, {entry, guildId, t: translate});
									if (!rendered && !shouldShowFallbackChangeDetail(change)) {
										return null;
									}
									const tone = getChangeTone(change);
									const ChangeIcon = getChangeIcon(tone);
									const toneClass = styles[`changeBullet_${tone}` as keyof typeof styles];

									return (
										<div className={styles.changeItem} key={`${entryId}-${change.key}-${changeIndex}`}>
											<span className={clsx(styles.changeBullet, toneClass)} aria-hidden>
												<ChangeIcon size={12} weight="bold" className={styles.changeBulletIcon} />
											</span>
											<span className={styles.changeText}>{rendered ?? renderFallbackChangeDetail(change)}</span>
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
								<div key={entryId} className={entryClasses}>
									{isExpandable ? (
										<button
											type="button"
											onClick={() => toggleExpanded(entryId)}
											className={headerClasses}
											aria-expanded={isExpandedView}
										>
											<span className={styles.icon} aria-hidden>
												<ActionIcon size={20} weight="bold" className={styles.iconGlyph} />
											</span>

											<div className={styles.avatar}>
												{actorUser ? (
													<Avatar user={actorUser} size={32} guildId={guildId} />
												) : (
													<MockAvatar size={32} userTag={entry.user_id ?? 'Unknown user'} />
												)}
											</div>

											<div className={styles.textBlock}>
												<div className={styles.titleRow}>
													<span className={styles.summary}>{summaryNode}</span>
												</div>
												<div className={styles.metaRow}>
													<span className={styles.timestamp}>{formatTimestamp(entry.id)}</span>
												</div>
											</div>

											<CaretDownIcon
												size={20}
												weight="bold"
												className={clsx(styles.chevron, {[styles.chevronExpanded]: isExpandedView})}
											/>
										</button>
									) : (
										<div className={headerClasses}>
											<span className={styles.icon} aria-hidden>
												<ActionIcon size={20} weight="bold" className={styles.iconGlyph} />
											</span>

											<div className={styles.avatar}>
												{actorUser ? (
													<Avatar user={actorUser} size={32} guildId={guildId} />
												) : (
													<MockAvatar size={32} userTag={entry.user_id ?? 'Unknown user'} />
												)}
											</div>

											<div className={styles.textBlock}>
												<div className={styles.titleRow}>
													<span className={styles.summary}>{summaryNode}</span>
												</div>
												<div className={styles.metaRow}>
													<span className={styles.timestamp}>{formatTimestamp(entry.id)}</span>
												</div>
											</div>
										</div>
									)}

									{isExpandedView && (
										<div className={styles.details}>
											{shouldShowReasonPreview && (
												<div className={styles.reasonRow}>
													<span className={styles.reasonLabel}>
														<Trans>Reason</Trans>
													</span>
													<span className={styles.reasonValue}>{reasonText}</span>
												</div>
											)}

											{(changeRows.length > 0 || optionEntries.length > 0) && (
												<div className={styles.changeList}>
													{changeRows}
													{optionEntries.map(([key, value]) => (
														<div className={styles.changeItem} key={key}>
															{(() => {
																const tone = getOptionTone(value);
																const ChangeIcon = getChangeIcon(tone);
																const toneClass = styles[`changeBullet_${tone}` as keyof typeof styles];
																return (
																	<span className={clsx(styles.changeBullet, toneClass)} aria-hidden>
																		<ChangeIcon size={12} weight="bold" className={styles.changeBulletIcon} />
																	</span>
																);
															})()}
															<span className={styles.changeText}>
																{renderOptionDetailSentence(
																	key,
																	value,
																	guildId,
																	entry.action_type as AuditLogActionType,
																	translate,
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
					<div className={styles.emptyState}>
						<EmptySlate
							Icon={ClipboardTextIcon}
							title={t`No logs yet`}
							description={t`When moderators begin moderating, you can moderate the moderation here.`}
						/>
					</div>
				)}

				{!isLoading && shouldShowErrorState && (
					<div className={styles.errorState}>
						<EmptySlate
							Icon={WarningCircleIcon}
							title={t`Unable to load activity logs`}
							description={errorDescription}
						/>
						<div className={styles.statusActions}>
							<Button variant="secondary" onClick={() => loadLogs({reset: true})}>
								{t`Retry`}
							</Button>
						</div>
					</div>
				)}

				{hasMore && entries.length > 0 && !shouldShowErrorState && (
					<div className={styles.loadMore}>
						<Button onClick={handleLoadMore} submitting={isLoading}>
							{t`Load more`}
						</Button>
					</div>
				)}
			</div>
		</div>
	);
});

export default GuildAuditLogTab;
