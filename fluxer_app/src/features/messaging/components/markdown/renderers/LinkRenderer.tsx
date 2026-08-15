// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {
	openOAuthAuthorizeModalFromUrl,
	parseOAuthAuthorizeModalUrl,
} from '@app/features/auth/commands/OAuthAuthorizeModalCommands';
import * as ChannelGateCommands from '@app/features/channel/commands/ChannelGateCommands';
import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {getDMDisplayName, getIcon, getName} from '@app/features/channel/utils/ChannelUtils';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import type {Guild} from '@app/features/guild/models/Guild';
import Guilds from '@app/features/guild/state/Guilds';
import {getGuildIconDisplayInitials} from '@app/features/guild/utils/GuildInitialsUtils';
import {
	COPY_LINK_DESCRIPTOR,
	OKAY_DESCRIPTOR,
	OPEN_LINK_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import jumpLinkStyles from '@app/features/messaging/components/markdown/renderers/MessageJumpLink.module.css';
import {MarkdownContext, type RendererProps} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {ExternalLinkWarningModal} from '@app/features/messaging/components/modals/ExternalLinkWarningModal';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import type {LinkNode} from '@app/features/messaging/utils/markdown/parser/Nodes';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {
	handleDeepLinkUrl,
	isInternalChannelHost,
	navigateToLinkedUserProfile,
	parseChannelJumpLink,
	parseChannelUrl,
	parseMessageJumpLink,
	parseUserProfileUrl,
	parseUserSettingsDeepLink,
} from '@app/features/navigation/utils/DeepLinkUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import * as ThemeCommands from '@app/features/theme/commands/ThemeCommands';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import * as ThemeUtils from '@app/features/theme/utils/ThemeUtils';
import TrustedDomain from '@app/features/trusted_domain/state/TrustedDomain';
import {CopyLinkIcon, OpenLinkIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {APP_PROTOCOL_PREFIX, APP_PROTOCOL_SCHEME, isAppProtocolUrl} from '@app/features/ui/utils/AppProtocol';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {
	getUserSettingsSectionLabel,
	getUserSettingsTabIconDescriptor,
	getUserSettingsTabLabel,
} from '@app/features/user/components/settings_utils/SettingsConstants';
import type {UserSettingsDeepLinkTarget} from '@app/features/user/components/settings_utils/UserSettingsDeepLinks';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as StringUtils from '@app/lib/strings';
import {ME} from '@fluxer/constants/src/AppConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react';
import {CaretRightIcon, ChatTeardropIcon, LockIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const MESSAGE_LINK_DESCRIPTOR = msg({
	message: 'message link',
	comment:
		'Type chip suffix in tooltips for inline rendered message links. Lowercase because it follows the channel name.',
});
const CHANNEL_LINK_DESCRIPTOR = msg({
	message: 'channel link',
	comment:
		'Type chip suffix in tooltips for inline rendered channel mentions. Lowercase because it follows the channel name.',
});
const JUMP_TO_THE_MESSAGE_IN_DESCRIPTOR = msg({
	message: 'Jump to the message in {labelText}',
	comment:
		'Accessible label for an inline message link that jumps to a specific message in a channel. labelText is the channel display name.',
});
const JUMP_TO_THE_LINKED_MESSAGE_DESCRIPTOR = msg({
	message: 'Jump to the linked message',
	comment: 'Fallback accessible label for an inline message link when the target channel is not known.',
});
const JUMP_TO_DESCRIPTOR = msg({
	message: 'Jump to {labelText}',
	comment: 'Accessible label for an inline channel mention link. labelText is the channel display name.',
});
const JUMP_TO_THE_LINKED_CHANNEL_DESCRIPTOR = msg({
	message: 'Jump to the linked channel',
	comment: 'Fallback accessible label for an inline channel mention when the target name is not known.',
});
const OPEN_SETTINGS_SECTION_DESCRIPTOR = msg({
	message: 'Open {labelText} in settings',
	comment:
		'Accessible label for an inline settings deep-link pill. labelText is the resolved settings tab and section breadcrumb.',
});
const SETTINGS_LINK_DESCRIPTOR = msg({
	message: 'settings link',
	comment: 'Type chip suffix in tooltips for inline rendered fluxer://settings/user deep links. Lowercase.',
});
const CHANNEL_ACCESS_DENIED_DESCRIPTOR = msg({
	message: 'Channel access denied',
	comment: 'Title of the alert shown when clicking a message link the user does not have permission to view.',
});
const YOU_DO_NOT_HAVE_ACCESS_TO_THE_CHANNEL_DESCRIPTOR = msg({
	message: 'You do not have access to the channel where this message was sent.',
	comment: 'Body of the access-denied alert for clicked message links.',
});
const NO_ACCESS_DESCRIPTOR = msg({
	message: 'No access',
	comment: 'Inline text shown inside a custom message link pill when the linked message cannot be accessed.',
});
const DEVELOPER_TOOLS_DESCRIPTOR = msg({
	message: 'Developer tools',
	comment: 'Title of the info alert shown when clicking an internal developer-tools deep link.',
});
const DEVELOPER_TOOLS_ARE_AVAILABLE_FROM_THE_STAFF_BADGE_DESCRIPTOR = msg({
	message: 'Developer tools are available from the staff badge in the channel header.',
	comment: 'Body of the developer-tools info alert. Refers to the staff badge button in the channel header.',
});
const LINK_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Link unavailable',
	comment: 'Title of the alert shown when clicking an internal link that is not available to the current account.',
});
const THIS_LINK_IS_NOT_AVAILABLE_FOR_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'This link is not available for your account.',
	comment: 'Body of the link-unavailable alert.',
});
const INVITE_LINK_MASKED_DESCRIPTOR = msg({
	message: 'Invite link masked while sharing',
	comment: 'Inline replacement text shown for invite links while streaming privacy is active.',
});
const logger = new Logger('LinkRenderer');

function showChannelAccessDeniedModal(i18n: I18n): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(CHANNEL_ACCESS_DENIED_DESCRIPTOR)}
				description={i18n._(YOU_DO_NOT_HAVE_ACCESS_TO_THE_CHANNEL_DESCRIPTOR)}
				primaryText={i18n._(OKAY_DESCRIPTOR)}
				primaryVariant="primary"
				secondaryText={false}
				onPrimary={() => {}}
				hideCloseButton
				data-flx="messaging.markdown.renderers.link-renderer.show-channel-access-denied-modal.confirm-modal"
			/>
		)),
	);
}

function getJumpGateGuildId(channel: Channel, scope?: string | null): string | null {
	if (channel.guildId) return channel.guildId;
	if (scope && scope !== ME) return scope;
	return null;
}

function promptForJumpGate(channel: Channel, scope: string | null | undefined, onConfirm: () => void): boolean {
	return ChannelGateCommands.promptForChannelGate({
		channel,
		channelId: channel.id,
		guildId: getJumpGateGuildId(channel, scope),
		onConfirm,
	});
}

type CompactGuildMentionIconStyle = React.CSSProperties & {
	'--jump-link-guild-icon-image'?: string;
};

function CompactGuildMentionIcon({guild}: {guild: Guild}) {
	const iconUrl = guild.icon ? AvatarUtils.getGuildIconURL({id: guild.id, icon: guild.icon}) : '';
	const style: CompactGuildMentionIconStyle | undefined = iconUrl
		? {'--jump-link-guild-icon-image': `url(${iconUrl})`}
		: undefined;
	const initials = getGuildIconDisplayInitials(StringUtils.getInitialsFromName(guild.name));
	return (
		<span
			className={clsx(jumpLinkStyles.icon, jumpLinkStyles.guildIcon)}
			style={style}
			aria-hidden="true"
			data-jump-link-guild-icon=""
			data-flx="messaging.markdown.renderers.link-renderer.compact-guild-mention-icon"
		>
			{!iconUrl && (
				<span
					className={jumpLinkStyles.guildInitials}
					data-flx="messaging.markdown.renderers.link-renderer.compact-guild-mention-icon.initials"
				>
					{initials}
				</span>
			)}
		</span>
	);
}

interface JumpLinkMentionProps {
	channel: Channel;
	guild: Guild | null;
	messageId?: string;
	returnTargetId?: string;
	returnChannelId?: string;
	url: string;
	i18n: I18n;
	interactive?: boolean;
}

interface JumpLinkContextMenuProps {
	url: string;
	i18n: I18n;
	onClose: () => void;
}

function JumpLinkContextMenu({url, i18n, onClose}: JumpLinkContextMenuProps) {
	const handleOpenLink = useCallback(() => {
		void openExternalUrl(url);
		onClose();
	}, [onClose, url]);
	const handleCopyLink = useCallback(async () => {
		await TextCopyCommands.copy(i18n, url);
		onClose();
	}, [i18n, onClose, url]);
	return (
		<MenuGroup data-flx="messaging.markdown.renderers.link-renderer.jump-link-context-menu.menu-group">
			<MenuItem
				icon={
					<OpenLinkIcon
						size={20}
						data-flx="messaging.markdown.renderers.link-renderer.jump-link-context-menu.open-link-icon"
					/>
				}
				onClick={handleOpenLink}
				data-flx="messaging.markdown.renderers.link-renderer.jump-link-context-menu.menu-item.open-link"
			>
				{i18n._(OPEN_LINK_DESCRIPTOR)}
			</MenuItem>
			<MenuItem
				icon={
					<CopyLinkIcon
						size={20}
						data-flx="messaging.markdown.renderers.link-renderer.jump-link-context-menu.copy-link-icon"
					/>
				}
				onClick={() => void handleCopyLink()}
				data-flx="messaging.markdown.renderers.link-renderer.jump-link-context-menu.menu-item.copy-link"
			>
				{i18n._(COPY_LINK_DESCRIPTOR)}
			</MenuItem>
		</MenuGroup>
	);
}

const JumpLinkMention = observer(function JumpLinkMention({
	channel,
	guild,
	messageId,
	returnTargetId,
	returnChannelId,
	url,
	i18n,
	interactive = true,
}: JumpLinkMentionProps) {
	useLingui();
	const navigateToJumpTarget = useCallback(() => {
		if (LinkChannelCommands.openLinkChannel(channel, {skipGate: true})) {
			return;
		}
		if (messageId) {
			goToMessage(channel.id, messageId, {returnTargetId, returnChannelId});
			return;
		}
		NavigationCommands.selectChannel(channel.guildId ?? undefined, channel.id);
	}, [channel, messageId, returnChannelId, returnTargetId]);
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			event.preventDefault();
			event.stopPropagation();
			if (promptForJumpGate(channel, guild?.id ?? null, navigateToJumpTarget)) {
				return;
			}
			navigateToJumpTarget();
		},
		[channel, guild?.id, interactive, navigateToJumpTarget],
	);
	const handleAuxClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			if (event.button !== 1) return;
			event.preventDefault();
			event.stopPropagation();
			if (url) void openExternalUrl(url);
		},
		[interactive, url],
	);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<JumpLinkContextMenu
					url={url}
					i18n={i18n}
					onClose={onClose}
					data-flx="messaging.markdown.renderers.link-renderer.handle-context-menu.jump-link-context-menu"
				/>
			));
		},
		[interactive, i18n, url],
	);
	const displayName = channel.isPrivate() ? getDMDisplayName(channel) : (channel.name ?? channel.id);
	const labelText = guild ? guild.name : displayName;
	const shouldShowChannelInfo = !messageId && Boolean(channel.guildId);
	const channelDisplayName = channel.name ?? getName(channel);
	const isDMChannel = channel.isPrivate() && !channel.guildId;
	const shouldShowDMIconLabel = isDMChannel && !messageId;
	const hasDetailChunk = Boolean(messageId) || shouldShowChannelInfo;
	const roleDescription = messageId ? i18n._(MESSAGE_LINK_DESCRIPTOR) : i18n._(CHANNEL_LINK_DESCRIPTOR);
	const ariaLabel = messageId
		? labelText
			? i18n._(JUMP_TO_THE_MESSAGE_IN_DESCRIPTOR, {labelText})
			: i18n._(JUMP_TO_THE_LINKED_MESSAGE_DESCRIPTOR)
		: labelText
			? i18n._(JUMP_TO_DESCRIPTOR, {labelText})
			: i18n._(JUMP_TO_THE_LINKED_CHANNEL_DESCRIPTOR);
	const Component = interactive ? 'button' : 'span';
	return (
		<Component
			data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.component.click"
			{...(interactive ? {type: 'button'} : {})}
			className={clsx(markupStyles.mention, interactive && markupStyles.interactive, jumpLinkStyles.jumpLinkButton)}
			onClick={handleClick}
			onAuxClick={handleAuxClick}
			onContextMenu={handleContextMenu}
			aria-label={ariaLabel}
			{...(interactive ? {'aria-roledescription': roleDescription} : {})}
			tabIndex={interactive ? 0 : -1}
		>
			{guild ? (
				<span
					className={jumpLinkStyles.part}
					data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.part--guild"
				>
					<CompactGuildMentionIcon
						guild={guild}
						data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.compact-guild-mention-icon"
					/>
					<span
						className={jumpLinkStyles.name}
						data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.guild-name"
					>
						{guild.name}
					</span>
				</span>
			) : shouldShowDMIconLabel ? (
				<span
					className={jumpLinkStyles.part}
					data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.part--dm"
				>
					{getIcon(channel, {className: jumpLinkStyles.icon})}
					<span
						className={jumpLinkStyles.name}
						data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.dm-name"
					>
						{displayName}
					</span>
				</span>
			) : (
				<span
					className={jumpLinkStyles.name}
					data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.label"
				>
					{displayName}
				</span>
			)}
			{hasDetailChunk && (
				<>
					<span
						className={jumpLinkStyles.divider}
						aria-hidden="true"
						data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.divider"
					>
						<CaretRightIcon
							weight="bold"
							data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.caret-right-icon"
						/>
					</span>
					{messageId ? (
						<ChatTeardropIcon
							className={jumpLinkStyles.icon}
							weight="fill"
							data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.chat-teardrop-icon"
						/>
					) : (
						shouldShowChannelInfo && (
							<span
								className={jumpLinkStyles.part}
								data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.part--channel"
							>
								{getIcon(channel, {className: jumpLinkStyles.icon})}
								<span
									className={jumpLinkStyles.name}
									data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention.channel-name"
								>
									{channelDisplayName}
								</span>
							</span>
						)
					)}
				</>
			)}
		</Component>
	);
});

interface InaccessibleJumpLinkMentionProps {
	url: string;
	i18n: I18n;
	interactive?: boolean;
}

function InaccessibleJumpLinkMention({url, i18n, interactive = true}: InaccessibleJumpLinkMentionProps) {
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			event.preventDefault();
			event.stopPropagation();
			showChannelAccessDeniedModal(i18n);
		},
		[i18n, interactive],
	);
	const handleAuxClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			if (event.button !== 1) return;
			event.preventDefault();
			event.stopPropagation();
			if (url) void openExternalUrl(url);
		},
		[interactive, url],
	);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<JumpLinkContextMenu
					url={url}
					i18n={i18n}
					onClose={onClose}
					data-flx="messaging.markdown.renderers.link-renderer.handle-context-menu.jump-link-context-menu--2"
				/>
			));
		},
		[interactive, i18n, url],
	);
	const noAccessText = i18n._(NO_ACCESS_DESCRIPTOR);
	const Component = interactive ? 'button' : 'span';
	return (
		<Component
			data-flx="messaging.markdown.renderers.link-renderer.inaccessible-jump-link-mention.component"
			{...(interactive ? {type: 'button'} : {})}
			className={clsx(markupStyles.mention, interactive && markupStyles.interactive, jumpLinkStyles.jumpLinkButton)}
			onClick={handleClick}
			onAuxClick={handleAuxClick}
			onContextMenu={handleContextMenu}
			aria-label={noAccessText}
			{...(interactive ? {'aria-roledescription': i18n._(MESSAGE_LINK_DESCRIPTOR)} : {})}
			tabIndex={interactive ? 0 : -1}
		>
			<LockIcon
				className={jumpLinkStyles.icon}
				weight="fill"
				data-flx="messaging.markdown.renderers.link-renderer.inaccessible-jump-link-mention.lock-icon"
			/>
			<span
				className={clsx(jumpLinkStyles.name, jumpLinkStyles.noAccess)}
				data-flx="messaging.markdown.renderers.link-renderer.inaccessible-jump-link-mention.text"
			>
				{noAccessText}
			</span>
		</Component>
	);
}

interface SettingsJumpLinkMentionProps {
	target: UserSettingsDeepLinkTarget;
	url: string;
	i18n: I18n;
	interactive?: boolean;
}

interface SettingsLinkContextMenuProps {
	url: string;
	i18n: I18n;
	onClose: () => void;
}

function SettingsLinkContextMenu({url, i18n, onClose}: SettingsLinkContextMenuProps) {
	const handleCopyLink = useCallback(async () => {
		await TextCopyCommands.copy(i18n, url);
		onClose();
	}, [i18n, onClose, url]);
	return (
		<MenuGroup data-flx="messaging.markdown.renderers.link-renderer.settings-link-context-menu.menu-group">
			<MenuItem
				icon={
					<CopyLinkIcon
						size={20}
						data-flx="messaging.markdown.renderers.link-renderer.settings-link-context-menu.copy-link-icon"
					/>
				}
				onClick={() => void handleCopyLink()}
				data-flx="messaging.markdown.renderers.link-renderer.settings-link-context-menu.menu-item.copy-link"
			>
				{i18n._(COPY_LINK_DESCRIPTOR)}
			</MenuItem>
		</MenuGroup>
	);
}

function SettingsJumpLinkMention({target, url, i18n, interactive = true}: SettingsJumpLinkMentionProps) {
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			event.preventDefault();
			event.stopPropagation();
			handleDeepLinkUrl(url);
		},
		[interactive, url],
	);
	const handleAuxClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			if (event.button !== 1) return;
			event.preventDefault();
			event.stopPropagation();
			if (url) void openExternalUrl(url);
		},
		[interactive, url],
	);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent<HTMLButtonElement | HTMLSpanElement>) => {
			if (!interactive) return;
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<SettingsLinkContextMenu
					url={url}
					i18n={i18n}
					onClose={onClose}
					data-flx="messaging.markdown.renderers.link-renderer.handle-context-menu.settings-link-context-menu"
				/>
			));
		},
		[interactive, i18n, url],
	);
	const tabLabel = getUserSettingsTabLabel(i18n, target.tab);
	const sectionLabel = target.section ? getUserSettingsSectionLabel(i18n, target.section, target.tab) : '';
	const tabIcon = getUserSettingsTabIconDescriptor(target.tab);
	const TabIcon = tabIcon?.icon;
	const tabIconWeight = tabIcon?.iconWeight ?? 'fill';
	const ariaLabelText = sectionLabel ? `${tabLabel} > ${sectionLabel}` : tabLabel;
	const Component = interactive ? 'button' : 'span';
	return (
		<Component
			data-flx="messaging.markdown.renderers.link-renderer.settings-jump-link-mention.component.click"
			{...(interactive ? {type: 'button'} : {})}
			className={clsx(markupStyles.mention, interactive && markupStyles.interactive, jumpLinkStyles.jumpLinkButton)}
			onClick={handleClick}
			onAuxClick={handleAuxClick}
			onContextMenu={handleContextMenu}
			aria-label={i18n._(OPEN_SETTINGS_SECTION_DESCRIPTOR, {labelText: ariaLabelText})}
			{...(interactive ? {'aria-roledescription': i18n._(SETTINGS_LINK_DESCRIPTOR)} : {})}
			tabIndex={interactive ? 0 : -1}
		>
			<span
				className={jumpLinkStyles.part}
				data-flx="messaging.markdown.renderers.link-renderer.settings-jump-link-mention.part--tab"
			>
				{TabIcon && (
					<TabIcon
						className={jumpLinkStyles.icon}
						weight={tabIconWeight}
						aria-hidden="true"
						data-flx="messaging.markdown.renderers.link-renderer.settings-jump-link-mention.tab-icon"
					/>
				)}
				<span
					className={jumpLinkStyles.name}
					data-flx="messaging.markdown.renderers.link-renderer.settings-jump-link-mention.tab-name"
				>
					{tabLabel}
				</span>
			</span>
			{sectionLabel && (
				<>
					<span
						className={jumpLinkStyles.divider}
						aria-hidden="true"
						data-flx="messaging.markdown.renderers.link-renderer.settings-jump-link-mention.divider"
					>
						<CaretRightIcon
							weight="bold"
							data-flx="messaging.markdown.renderers.link-renderer.settings-jump-link-mention.caret-right-icon"
						/>
					</span>
					<span
						className={jumpLinkStyles.name}
						data-flx="messaging.markdown.renderers.link-renderer.settings-jump-link-mention.section-name"
					>
						{sectionLabel}
					</span>
				</>
			)}
		</Component>
	);
}

export const LinkRenderer = observer(function LinkRenderer({
	node,
	id,
	renderChildren,
	options,
}: RendererProps<LinkNode>): React.ReactElement {
	const i18n = options.i18n!;
	const {url, text} = node;
	const content = text ? renderChildren([text]) : url;
	const inviteCode = InviteUtils.findInvite(url);
	const themeCode = ThemeUtils.findTheme(url);
	const userProfileId = parseUserProfileUrl(url);
	const messageJumpTarget = parseMessageJumpLink(url);
	const channelJumpTarget = messageJumpTarget ? null : parseChannelJumpLink(url);
	const jumpTarget = messageJumpTarget ?? channelJumpTarget;
	const jumpChannel = jumpTarget ? (Channels.getChannel(jumpTarget.channelId) ?? null) : null;
	const jumpGuild = jumpChannel?.guildId ? (Guilds.getGuild(jumpChannel.guildId) ?? null) : null;
	const settingsTarget = isAppProtocolUrl(url) ? parseUserSettingsDeepLink(url) : null;
	const isInlineReplyContext = options.context === MarkdownContext.RESTRICTED_INLINE_REPLY;
	const shouldDisableInteractions = options.disableInteractions === true;
	if (inviteCode && StreamerMode.shouldHideInviteLinks) {
		return (
			<span key={id} className={markupStyles.link} data-flx="messaging.markdown.renderers.link-renderer.invite-masked">
				{i18n._(INVITE_LINK_MASKED_DESCRIPTOR)}
			</span>
		);
	}
	if (settingsTarget && !text) {
		const mention = (
			<SettingsJumpLinkMention
				target={settingsTarget}
				url={url}
				i18n={i18n}
				interactive={!isInlineReplyContext && !shouldDisableInteractions}
				data-flx="messaging.markdown.renderers.link-renderer.settings-jump-link-mention"
			/>
		);
		return shouldDisableInteractions || isInlineReplyContext ? (
			mention
		) : (
			<FocusRing key={id} offset={-2} data-flx="messaging.markdown.renderers.link-renderer.focus-ring--settings">
				{mention}
			</FocusRing>
		);
	}
	if (jumpTarget && jumpChannel && !text) {
		const mention = (
			<JumpLinkMention
				channel={jumpChannel}
				guild={jumpGuild}
				messageId={messageJumpTarget?.messageId}
				returnTargetId={options.messageId}
				returnChannelId={options.channelId}
				url={url}
				i18n={i18n}
				interactive={!isInlineReplyContext && !shouldDisableInteractions}
				data-flx="messaging.markdown.renderers.link-renderer.jump-link-mention"
			/>
		);
		return shouldDisableInteractions || isInlineReplyContext ? (
			mention
		) : (
			<FocusRing key={id} offset={-2} data-flx="messaging.markdown.renderers.link-renderer.focus-ring">
				{mention}
			</FocusRing>
		);
	}
	if (jumpTarget && !jumpChannel && !text) {
		const mention = (
			<InaccessibleJumpLinkMention
				url={url}
				i18n={i18n}
				interactive={!isInlineReplyContext && !shouldDisableInteractions}
				data-flx="messaging.markdown.renderers.link-renderer.inaccessible-jump-link-mention"
			/>
		);
		return shouldDisableInteractions || isInlineReplyContext ? (
			mention
		) : (
			<FocusRing key={id} offset={-2} data-flx="messaging.markdown.renderers.link-renderer.focus-ring--inaccessible">
				{mention}
			</FocusRing>
		);
	}
	if (shouldDisableInteractions) {
		return (
			<span key={id} className={markupStyles.link} data-flx="messaging.markdown.renderers.link-renderer.span">
				{content}
			</span>
		);
	}
	const shouldShowAccessDeniedModal = Boolean(jumpTarget && !jumpChannel);
	let isInternal = false;
	let handleClick: ((e: React.MouseEvent) => void) | undefined;
	if (userProfileId) {
		handleClick = (event) => {
			event.preventDefault();
			event.stopPropagation();
			navigateToLinkedUserProfile(userProfileId);
		};
		isInternal = true;
	} else if (shouldShowAccessDeniedModal) {
		handleClick = (event) => {
			event.preventDefault();
			event.stopPropagation();
			showChannelAccessDeniedModal(i18n);
		};
		isInternal = true;
	} else if (url === `${APP_PROTOCOL_PREFIX}dev` || url === `${APP_PROTOCOL_SCHEME}dev`) {
		handleClick = (e) => {
			e.preventDefault();
			if (DeveloperMode.isDeveloper) {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(DEVELOPER_TOOLS_DESCRIPTOR)}
							description={i18n._(DEVELOPER_TOOLS_ARE_AVAILABLE_FROM_THE_STAFF_BADGE_DESCRIPTOR)}
							primaryText={i18n._(OKAY_DESCRIPTOR)}
							primaryVariant="primary"
							secondaryText={false}
							onPrimary={() => {}}
							hideCloseButton
							data-flx="messaging.markdown.renderers.link-renderer.handle-click.confirm-modal--2"
						/>
					)),
				);
			} else {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(LINK_UNAVAILABLE_DESCRIPTOR)}
							description={i18n._(THIS_LINK_IS_NOT_AVAILABLE_FOR_YOUR_ACCOUNT_DESCRIPTOR)}
							primaryText={i18n._(OKAY_DESCRIPTOR)}
							primaryVariant="primary"
							secondaryText={false}
							onPrimary={() => {}}
							hideCloseButton
							data-flx="messaging.markdown.renderers.link-renderer.handle-click.confirm-modal--3"
						/>
					)),
				);
			}
		};
		isInternal = true;
	} else if (isAppProtocolUrl(url)) {
		handleClick = (event) => {
			event.preventDefault();
			event.stopPropagation();
			if (handleDeepLinkUrl(url)) return;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(LINK_UNAVAILABLE_DESCRIPTOR)}
						description={i18n._(THIS_LINK_IS_NOT_AVAILABLE_FOR_YOUR_ACCOUNT_DESCRIPTOR)}
						primaryText={i18n._(OKAY_DESCRIPTOR)}
						primaryVariant="primary"
						secondaryText={false}
						onPrimary={() => {}}
						hideCloseButton
						data-flx="messaging.markdown.renderers.link-renderer.handle-click.confirm-modal--4"
					/>
				)),
			);
		};
		isInternal = true;
	} else {
		try {
			const oauthAuthorizeUrl = parseOAuthAuthorizeModalUrl(url);
			const parsed = oauthAuthorizeUrl ?? new URL(url);
			if (oauthAuthorizeUrl) {
				handleClick = (e) => {
					e.preventDefault();
					e.stopPropagation();
					if (!openOAuthAuthorizeModalFromUrl(url)) {
						void openExternalUrl(url);
					}
				};
				isInternal = true;
			} else {
				isInternal = isInternalChannelHost(parsed.host) && parsed.pathname.startsWith('/channels/');
			}
			if (!handleClick && inviteCode) {
				handleClick = (e) => {
					e.preventDefault();
					void InviteCommands.openAcceptModal(inviteCode);
				};
				isInternal = true;
			} else if (!handleClick && themeCode) {
				handleClick = (e) => {
					e.preventDefault();
					ThemeCommands.openAcceptModal(themeCode, i18n);
				};
				isInternal = true;
			} else if (!handleClick && isInternal) {
				const channelJump = channelJumpTarget;
				if (messageJumpTarget && jumpChannel) {
					const targetChannelId = messageJumpTarget.channelId;
					const targetMessageId = messageJumpTarget.messageId;
					handleClick = (e) => {
						e.preventDefault();
						const navigateToTarget = () => {
							if (LinkChannelCommands.openLinkChannel(jumpChannel, {skipGate: true})) {
								return;
							}
							goToMessage(targetChannelId, targetMessageId, {
								returnTargetId: options.messageId,
								returnChannelId: options.channelId,
							});
						};
						if (promptForJumpGate(jumpChannel, messageJumpTarget.scope, navigateToTarget)) {
							return;
						}
						navigateToTarget();
					};
				} else if (channelJump) {
					handleClick = (e) => {
						e.preventDefault();
						const navigateToTarget = () => {
							if (jumpChannel && LinkChannelCommands.openLinkChannel(jumpChannel, {skipGate: true})) {
								return;
							}
							NavigationCommands.selectChannel(
								channelJump.scope === ME ? undefined : channelJump.scope,
								channelJump.channelId,
							);
						};
						if (jumpChannel && promptForJumpGate(jumpChannel, channelJump.scope, navigateToTarget)) {
							return;
						}
						navigateToTarget();
					};
				} else if (parseChannelUrl(url)) {
					handleClick = (e) => {
						e.preventDefault();
						NavigationCommands.deselectGuild();
					};
				} else {
					isInternal = false;
				}
			}
			if (!isInternal && !inviteCode) {
				const isTrusted = TrustedDomain.isTrustedDomain(parsed.hostname);
				if (!isTrusted) {
					handleClick = (e) => {
						e.preventDefault();
						ModalCommands.push(
							modal(() => (
								<ExternalLinkWarningModal
									url={url}
									data-flx="messaging.markdown.renderers.link-renderer.handle-click.external-link-warning-modal"
								/>
							)),
						);
					};
				}
			}
		} catch (_error) {
			logger.warn('Invalid URL in link:', url);
		}
	}
	return (
		<FocusRing key={id} offset={-2} data-flx="messaging.markdown.renderers.link-renderer.focus-ring--2">
			<a
				href={url}
				target={isInternal ? undefined : '_blank'}
				rel={isInternal ? undefined : 'noopener noreferrer'}
				onClick={(e) => {
					e.stopPropagation();
					if (handleClick) {
						handleClick(e);
						return;
					}
					if (!isInternal) {
						e.preventDefault();
						void openExternalUrl(url);
					}
				}}
				onAuxClick={(e) => {
					if (e.button !== 1 || isInternal) {
						return;
					}
					e.preventDefault();
					e.stopPropagation();
					openExternalUrlWithWarning(url);
				}}
				className={markupStyles.link}
				data-flx="messaging.markdown.renderers.link-renderer.a"
			>
				{content}
			</a>
		</FocusRing>
	);
});
