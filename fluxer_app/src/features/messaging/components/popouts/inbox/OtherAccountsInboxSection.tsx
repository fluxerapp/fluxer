// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import AccountEngine from '@app/features/account_engine/state/AccountEngine';
import {SENSITIVE_MESSAGE_PREVIEW_DESCRIPTOR} from '@app/features/account_engine/state/AccountEngineMessages';
import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import {resolveAccountInstanceLabel} from '@app/features/auth/AccountDisplayUtils';
import {switcherAccounts} from '@app/features/auth/state/AccountSwitcherAccounts';
import Accounts from '@app/features/auth/state/Accounts';
import OtherAccountsInbox from '@app/features/inbox/state/OtherAccountsInbox';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import styles from '@app/features/messaging/components/popouts/inbox/OtherAccountsInboxSection.module.css';
import {getParserFlagsForContext} from '@app/features/messaging/utils/markdown/MarkdownParserFlags';
import {parseAndRenderToPlaintext} from '@app/features/messaging/utils/markdown/Plaintext';
import {switchToOtherAccount} from '@app/features/search/commands/QuickSwitcherCommands';
import {
	OtherAccountAvatar,
	OtherAccountChannelIcon,
} from '@app/features/search/components/quick_switcher/OtherAccountAvatar';
import {accountChannelTitle} from '@app/features/search/state/QuickSwitcherAccountResults';
import {
	accountsSpanSeveralInstances,
	otherAccountDisplayName,
} from '@app/features/search/state/QuickSwitcherOtherAccounts';
import {AvatarStack} from '@app/features/ui/avatars/AvatarStack';
import {Button} from '@app/features/ui/button/Button';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {MockAvatar} from '@app/features/ui/components/MockAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getDefaultAvatarURLForIndex} from '@app/features/user/utils/AvatarUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {
	AccountEngineAccountActivity,
	AccountEngineChannelActivity,
	AccountEngineMention,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, CheckIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {reaction} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useId, useMemo} from 'react';

const OTHER_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Other accounts',
	comment: 'Heading of the inbox section that lists unread channels and mentions on the other signed-in accounts.',
});
const MENTIONS_ON_ACCOUNTS_DESCRIPTOR = msg({
	message:
		'{mentionCount, plural, one {# mention} other {# mentions}} on {accountCount, plural, one {# other account} other {# other accounts}}',
	comment:
		'Summary in the inbox for the other signed-in accounts. mentionCount is the total number of mentions and accountCount is how many other accounts have mentions.',
});
const UNREAD_ON_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Unread messages on {accountCount, plural, one {# other account} other {# other accounts}}',
	comment:
		'Summary in the inbox when the other signed-in accounts have unread messages but no mentions. accountCount is how many other accounts have activity.',
});
const SHOW_OTHER_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Show activity on other accounts',
	comment: 'Accessible label for the button that expands the other accounts section in the inbox.',
});
const HIDE_OTHER_ACCOUNTS_DESCRIPTOR = msg({
	message: 'Hide activity on other accounts',
	comment: 'Accessible label for the button that collapses the other accounts section in the inbox.',
});
const SWITCH_DESCRIPTOR = msg({
	message: 'Switch',
	comment: 'Button in the inbox that switches to another signed-in account.',
});
const SWITCH_TO_ACCOUNT_DESCRIPTOR = msg({
	message: 'Switch to {accountName}',
	comment: 'Accessible label for the switch button in the inbox. accountName is the account display name.',
});
const MARK_AS_READ_DESCRIPTOR = msg({
	message: 'Mark as read',
	comment: 'Tooltip and accessible label for the button that marks a channel on another account as read.',
});
const MARK_CHANNEL_AS_READ_DESCRIPTOR = msg({
	message: 'Mark {channelName} as read on {accountName}',
	comment:
		'Accessible label for marking a channel on another account as read. channelName is the channel name and accountName is the account display name.',
});
const OPEN_CHANNEL_DESCRIPTOR = msg({
	message: '{channelName} in {communityName} on {accountName}',
	comment:
		'Accessible label for an inbox row that opens a channel on another account. channelName, communityName and accountName are names.',
});
const OPEN_DM_DESCRIPTOR = msg({
	message: '{channelName} on {accountName}',
	comment:
		'Accessible label for an inbox row that opens a direct message on another account. channelName is the conversation name and accountName is the account display name.',
});
const MENTION_COUNT_DESCRIPTOR = msg({
	message: '{mentionCount, plural, one {# mention} other {# mentions}}',
	comment: 'Accessible label suffix for the mention count on an inbox row for another account.',
});
const UNREAD_MESSAGES_DESCRIPTOR = msg({
	message: 'Unread messages',
	comment: 'Accessible label suffix on an inbox row for another account with unread messages and no mentions.',
});
const RECENT_MENTIONS_DESCRIPTOR = msg({
	message: 'Recent mentions',
	comment: 'Small heading above the newest mentions of another account in the inbox.',
});
const OPEN_MENTION_DESCRIPTOR = msg({
	message: 'Mention from {authorName} in {channelName} on {accountName}',
	comment:
		'Accessible label for an inbox row that opens a mention on another account. authorName, channelName and accountName are names.',
});
const CHANNEL_IN_COMMUNITY_DESCRIPTOR = msg({
	message: '{channelName} · {communityName}',
	comment:
		'Context under a mention from another account in the inbox. channelName is the channel name and communityName is the community name. Keep the middle dot between the two names.',
});
const DIRECT_MESSAGE_DESCRIPTOR = msg({
	message: 'Direct message',
	comment: 'Context shown under a mention from a direct message on another account in the inbox.',
});

const COLLAPSED_AVATAR_LIMIT = 5;
const MENTION_PREVIEW_FLAGS = getParserFlagsForContext(MarkdownContext.STANDARD_WITHOUT_JUMBO);
const ROW_SELECTOR = '[data-other-accounts-row]';
const GROUPS_SELECTOR = '[data-other-accounts-groups]';

function closeInboxAndSwitch(accountKey: string, redirectAfterSwitch?: string): void {
	PopoutCommands.close();
	void switchToOtherAccount(accountKey, redirectAfterSwitch);
}

function channelRoute(channel: {guildId: string | null; channelId: string}): string {
	return Routes.guildChannel(channel.guildId ?? ME, channel.channelId);
}

function isDirectChannelType(type: number | null): boolean {
	return type === ChannelTypes.DM || type === ChannelTypes.GROUP_DM;
}

function mentionAvatarURL(accountKey: string, mention: AccountEngineMention): string {
	const instance = Accounts.getAccount(accountKey)?.instance ?? null;
	const instanceKey = instance == null ? null : runtimeInstanceKey(instance);
	const author = {id: mention.author.id, avatar: mention.author.avatar};
	try {
		if (instance?.mediaEndpoint && instanceKey != null) {
			return AvatarUtils.getUserAvatarURLForInstance(
				author,
				{mediaEndpoint: instance.mediaEndpoint, instanceKey},
				false,
				48,
			);
		}
		return AvatarUtils.getUserAvatarURL({id: author.id, avatar: null}, false, 48);
	} catch {
		return getDefaultAvatarURLForIndex(0);
	}
}

function mentionPreview(mention: AccountEngineMention, i18n: I18n): string {
	if (mention.gated) return i18n._(SENSITIVE_MESSAGE_PREVIEW_DESCRIPTOR);
	if (mention.content.length === 0) return '';
	return parseAndRenderToPlaintext(mention.content, MENTION_PREVIEW_FLAGS, {
		channelId: mention.channelId,
		preserveMarkdown: true,
		includeEmojiNames: true,
		resolveFromStores: false,
		i18n,
	});
}

function moveRowFocus(event: React.KeyboardEvent<HTMLElement>): void {
	if (event.key !== 'ArrowDown' && event.key !== 'ArrowUp' && event.key !== 'Home' && event.key !== 'End') return;
	const container = event.currentTarget.closest(GROUPS_SELECTOR);
	if (container == null) return;
	const rows = Array.from(container.querySelectorAll<HTMLElement>(ROW_SELECTOR));
	if (rows.length === 0) return;
	const current = rows.indexOf(event.currentTarget);
	let next: number;
	if (event.key === 'Home') next = 0;
	else if (event.key === 'End') next = rows.length - 1;
	else if (current === -1) next = 0;
	else next = Math.min(rows.length - 1, Math.max(0, current + (event.key === 'ArrowDown' ? 1 : -1)));
	event.preventDefault();
	rows[next]?.focus();
}

interface AccountGroupProps {
	activity: AccountEngineAccountActivity;
	showHost: boolean;
}

const ChannelRow = observer(
	({
		accountKey,
		accountName,
		channel,
	}: {
		accountKey: string;
		accountName: string;
		channel: AccountEngineChannelActivity;
	}) => {
		const {i18n} = useLingui();
		const title = accountChannelTitle(channel, i18n);
		const communityName = isDirectChannelType(channel.type) ? null : channel.guildName;
		const label = [
			communityName == null
				? i18n._(OPEN_DM_DESCRIPTOR, {channelName: title, accountName})
				: i18n._(OPEN_CHANNEL_DESCRIPTOR, {channelName: title, communityName, accountName}),
			channel.mentionCount > 0
				? i18n._(MENTION_COUNT_DESCRIPTOR, {mentionCount: channel.mentionCount})
				: i18n._(UNREAD_MESSAGES_DESCRIPTOR),
		].join(', ');
		const markLabel = i18n._(MARK_CHANNEL_AS_READ_DESCRIPTOR, {channelName: title, accountName});
		return (
			<li className={styles.row} data-flx="messaging.inbox.other-accounts.channel-row">
				<FocusRing offset={-2} data-flx="messaging.inbox.other-accounts.channel-row.focus-ring">
					<button
						type="button"
						className={styles.rowButton}
						aria-label={label}
						onClick={() => closeInboxAndSwitch(accountKey, channelRoute(channel))}
						onKeyDown={moveRowFocus}
						data-other-accounts-row=""
						data-flx="messaging.inbox.other-accounts.channel-row.jump"
					>
						<OtherAccountChannelIcon
							accountKey={accountKey}
							channelType={channel.type}
							highlighted
							data-flx="messaging.inbox.other-accounts.channel-row.icon"
						/>
						<span className={styles.rowText} data-flx="messaging.inbox.other-accounts.channel-row.text">
							<span className={styles.rowTitle} dir="auto" data-flx="messaging.inbox.other-accounts.channel-row.title">
								{title}
							</span>
							{communityName != null && (
								<span
									className={styles.rowSubtitle}
									dir="auto"
									data-flx="messaging.inbox.other-accounts.channel-row.subtitle"
								>
									{communityName}
								</span>
							)}
						</span>
						{channel.mentionCount > 0 ? (
							<MentionBadge
								mentionCount={channel.mentionCount}
								size="small"
								data-flx="messaging.inbox.other-accounts.channel-row.mention-badge"
							/>
						) : (
							<span
								className={styles.unreadDot}
								aria-hidden="true"
								data-flx="messaging.inbox.other-accounts.channel-row.unread-dot"
							/>
						)}
					</button>
				</FocusRing>
				<FocusRing offset={-2} data-flx="messaging.inbox.other-accounts.channel-row.mark-read.focus-ring">
					<Tooltip
						text={i18n._(MARK_AS_READ_DESCRIPTOR)}
						position="top"
						data-flx="messaging.inbox.other-accounts.channel-row.tooltip"
					>
						<button
							type="button"
							className={styles.iconButton}
							aria-label={markLabel}
							disabled={channel.lastMessageId == null}
							onClick={() => void OtherAccountsInbox.markAsRead(accountKey, channel)}
							data-flx="messaging.inbox.other-accounts.channel-row.mark-read"
						>
							<CheckIcon
								weight="bold"
								className={styles.icon}
								data-flx="messaging.inbox.other-accounts.channel-row.check-icon"
							/>
						</button>
					</Tooltip>
				</FocusRing>
			</li>
		);
	},
);

const MentionRow = ({
	accountKey,
	accountName,
	mention,
}: {
	accountKey: string;
	accountName: string;
	mention: AccountEngineMention;
}) => {
	const {i18n} = useLingui();
	const authorName = mention.author.globalName ?? mention.author.username;
	const channelName =
		mention.channelName ?? (isDirectChannelType(mention.channelType) ? i18n._(DIRECT_MESSAGE_DESCRIPTOR) : accountName);
	const context =
		mention.guildName == null
			? channelName
			: i18n._(CHANNEL_IN_COMMUNITY_DESCRIPTOR, {channelName, communityName: mention.guildName});
	const preview = useMemo(() => mentionPreview(mention, i18n), [mention, i18n]);
	const avatarUrl = useMemo(() => mentionAvatarURL(accountKey, mention), [accountKey, mention]);
	return (
		<li className={styles.mention} data-flx="messaging.inbox.other-accounts.mention-row">
			<FocusRing offset={-2} data-flx="messaging.inbox.other-accounts.mention-row.focus-ring">
				<button
					type="button"
					className={styles.mentionButton}
					aria-label={i18n._(OPEN_MENTION_DESCRIPTOR, {authorName, channelName, accountName})}
					onClick={() =>
						closeInboxAndSwitch(
							accountKey,
							Routes.channelMessage(mention.guildId ?? ME, mention.channelId, mention.messageId),
						)
					}
					onKeyDown={moveRowFocus}
					data-other-accounts-row=""
					data-flx="messaging.inbox.other-accounts.mention-row.jump"
				>
					<MockAvatar
						size={32}
						avatarUrl={avatarUrl}
						userTag={authorName}
						data-flx="messaging.inbox.other-accounts.mention-row.avatar"
					/>
					<span className={styles.mentionBody} data-flx="messaging.inbox.other-accounts.mention-row.body">
						<span className={styles.mentionMeta} data-flx="messaging.inbox.other-accounts.mention-row.meta">
							<span
								className={styles.mentionAuthor}
								dir="auto"
								data-flx="messaging.inbox.other-accounts.mention-row.author"
							>
								{authorName}
							</span>
							<span
								className={styles.mentionContext}
								dir="auto"
								data-flx="messaging.inbox.other-accounts.mention-row.context"
							>
								{context}
							</span>
						</span>
						{preview.length > 0 && (
							<span
								className={styles.mentionPreview}
								dir="auto"
								data-flx="messaging.inbox.other-accounts.mention-row.preview"
							>
								{preview}
							</span>
						)}
					</span>
				</button>
			</FocusRing>
		</li>
	);
};

const AccountGroup = observer(({activity, showHost}: AccountGroupProps) => {
	const {i18n} = useLingui();
	const headingId = useId();
	const account = Accounts.getAccount(activity.accountKey);
	if (account == null) return null;
	const accountName = otherAccountDisplayName(account, i18n);
	const host = showHost ? resolveAccountInstanceLabel(account) : null;
	const channels = OtherAccountsInbox.visibleChannels(activity);
	return (
		<section className={styles.group} aria-labelledby={headingId} data-flx="messaging.inbox.other-accounts.group">
			<div className={styles.groupHeader} data-flx="messaging.inbox.other-accounts.group.header">
				<OtherAccountAvatar
					accountKey={activity.accountKey}
					size={24}
					data-flx="messaging.inbox.other-accounts.group.avatar"
				/>
				<h3 id={headingId} className={styles.groupTitle} data-flx="messaging.inbox.other-accounts.group.title">
					<span className={styles.groupName} dir="auto" data-flx="messaging.inbox.other-accounts.group.name">
						{accountName}
					</span>
					{host != null && (
						<span className={styles.groupHost} dir="auto" data-flx="messaging.inbox.other-accounts.group.host">
							{host}
						</span>
					)}
				</h3>
				<Button
					variant="secondary"
					small
					compact
					aria-label={i18n._(SWITCH_TO_ACCOUNT_DESCRIPTOR, {accountName})}
					onClick={() => closeInboxAndSwitch(activity.accountKey)}
					data-flx="messaging.inbox.other-accounts.group.switch"
				>
					{i18n._(SWITCH_DESCRIPTOR)}
				</Button>
			</div>
			{channels.length > 0 && (
				<ul className={styles.rows} data-flx="messaging.inbox.other-accounts.group.rows">
					{channels.map((channel) => (
						<ChannelRow
							key={channel.channelId}
							accountKey={activity.accountKey}
							accountName={accountName}
							channel={channel}
							data-flx="messaging.inbox.other-accounts.group.channel-row"
						/>
					))}
				</ul>
			)}
			{activity.mentions.length > 0 && (
				<>
					<div className={styles.subheading} data-flx="messaging.inbox.other-accounts.group.mentions-heading">
						{i18n._(RECENT_MENTIONS_DESCRIPTOR)}
					</div>
					<ul className={styles.rows} data-flx="messaging.inbox.other-accounts.group.mentions">
						{activity.mentions.map((mention) => (
							<MentionRow
								key={mention.messageId}
								accountKey={activity.accountKey}
								accountName={accountName}
								mention={mention}
								data-flx="messaging.inbox.other-accounts.group.mention-row"
							/>
						))}
					</ul>
				</>
			)}
		</section>
	);
});

export const OtherAccountsInboxSection = observer(() => {
	const {i18n} = useLingui();
	const regionId = useId();
	const available = AccountEngine.available;
	const accountsWithActivity = AccountEngine.otherAccountsWithActivity;
	const collapsed = OtherAccountsInbox.collapsed;
	const isVisible = available && accountsWithActivity.length > 0;
	useEffect(() => {
		if (!isVisible || collapsed) return;
		const dispose = reaction(
			() =>
				Array.from(AccountEngine.summaries.values(), (summary) => `${summary.accountKey}:${summary.revision}`).join(
					'|',
				),
			() => void OtherAccountsInbox.load(),
			{fireImmediately: true},
		);
		return () => {
			dispose();
			OtherAccountsInbox.reset();
		};
	}, [isVisible, collapsed]);
	const handleToggle = useCallback(() => OtherAccountsInbox.toggleCollapsed(), []);
	if (!isVisible) return null;
	const mentionCount = AccountEngine.otherAccountsMentionCount;
	const summaryText =
		mentionCount > 0
			? i18n._(MENTIONS_ON_ACCOUNTS_DESCRIPTOR, {
					mentionCount,
					accountCount: AccountEngine.otherAccountsWithMentionsCount,
				})
			: i18n._(UNREAD_ON_ACCOUNTS_DESCRIPTOR, {accountCount: accountsWithActivity.length});
	const showHost = accountsSpanSeveralInstances(switcherAccounts());
	const activityByKey = new Map(OtherAccountsInbox.visibleAccounts.map((entry) => [entry.accountKey, entry]));
	const groups = accountsWithActivity
		.map((summary) => activityByKey.get(summary.accountKey))
		.filter((entry): entry is AccountEngineAccountActivity => entry != null);
	return (
		<section
			className={styles.section}
			aria-label={i18n._(OTHER_ACCOUNTS_DESCRIPTOR)}
			data-flx="messaging.inbox.other-accounts.section"
		>
			<FocusRing offset={-2} data-flx="messaging.inbox.other-accounts.summary.focus-ring">
				<button
					type="button"
					className={styles.summary}
					aria-expanded={!collapsed}
					aria-controls={regionId}
					aria-label={`${summaryText}. ${i18n._(collapsed ? SHOW_OTHER_ACCOUNTS_DESCRIPTOR : HIDE_OTHER_ACCOUNTS_DESCRIPTOR)}`}
					onClick={handleToggle}
					data-flx="messaging.inbox.other-accounts.summary.toggle"
				>
					<span className={styles.stack} aria-hidden="true" data-flx="messaging.inbox.other-accounts.summary.stack">
						<AvatarStack
							size={24}
							maxVisible={COLLAPSED_AVATAR_LIMIT}
							className={styles.stackAvatars}
							data-flx="messaging.inbox.other-accounts.summary.avatar-stack"
						>
							{accountsWithActivity.map((summary) => (
								<OtherAccountAvatar
									key={summary.accountKey}
									accountKey={summary.accountKey}
									size={24}
									data-flx="messaging.inbox.other-accounts.summary.stack-avatar"
								/>
							))}
						</AvatarStack>
					</span>
					<span className={styles.summaryText} data-flx="messaging.inbox.other-accounts.summary.text">
						{summaryText}
					</span>
					<CaretDownIcon
						weight="bold"
						className={clsx(styles.caret, collapsed && styles.caretCollapsed)}
						data-flx="messaging.inbox.other-accounts.summary.caret"
					/>
				</button>
			</FocusRing>
			{!collapsed && (
				<div
					id={regionId}
					className={styles.groups}
					data-other-accounts-groups=""
					data-flx="messaging.inbox.other-accounts.groups"
				>
					{groups.map((activity) => (
						<AccountGroup
							key={activity.accountKey}
							activity={activity}
							showHost={showHost}
							data-flx="messaging.inbox.other-accounts.group"
						/>
					))}
				</div>
			)}
		</section>
	);
});
