// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	openOAuthAuthorizeModalFromUrl,
	parseOAuthAuthorizeModalUrl,
} from '@app/features/auth/commands/OAuthAuthorizeModalCommands';
import * as ChannelGateCommands from '@app/features/channel/commands/ChannelGateCommands';
import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import DeveloperMode from '@app/features/devtools/state/DeveloperMode';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {navigateToAppPage, parseAppPageLink} from '@app/features/navigation/utils/AppPageLinks';
import {
	handleDeepLinkUrl,
	isInternalChannelHost,
	parseChannelJumpLink,
	parseChannelUrl,
	parseMessageJumpLink,
	parseUserProfileUrl,
	parseUserSettingsDeepLink,
} from '@app/features/navigation/utils/DeepLinkUtils';
import {currentInstanceTarget} from '@app/features/platform/transport/InstanceHTTP';
import * as ThemeCommands from '@app/features/theme/commands/ThemeCommands';
import * as ThemeUtils from '@app/features/theme/utils/ThemeUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {APP_PROTOCOL_PREFIX, APP_PROTOCOL_SCHEME, isAppProtocolUrl} from '@app/features/ui/utils/AppProtocol';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {ME} from '@fluxer/constants/src/AppConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const CHANNEL_ACCESS_DENIED_DESCRIPTOR = msg({
	message: 'Channel access denied',
	comment: 'Title of the alert shown when clicking a message link the user does not have permission to view.',
});
const YOU_DO_NOT_HAVE_ACCESS_TO_THE_CHANNEL_DESCRIPTOR = msg({
	message: 'You do not have access to the channel where this message was sent.',
	comment: 'Body of the access-denied alert for clicked message links.',
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

export function showChannelAccessDeniedModal(i18n: I18n): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(CHANNEL_ACCESS_DENIED_DESCRIPTOR)}
				description={i18n._(YOU_DO_NOT_HAVE_ACCESS_TO_THE_CHANNEL_DESCRIPTOR)}
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

export function promptForJumpGate(channel: Channel, scope: string | null | undefined, onConfirm: () => void): boolean {
	return ChannelGateCommands.promptForChannelGate({
		channel,
		channelId: channel.id,
		guildId: getJumpGateGuildId(channel, scope),
		onConfirm,
	});
}

export interface InAppLinkContext {
	i18n: I18n;
	guildId?: string;
	returnToMessageId?: string;
	returnChannelId?: string;
}

type InAppLinkAction = () => void;

export function getInAppLinkAction(url: string, context: InAppLinkContext): InAppLinkAction | null {
	const {i18n} = context;
	const userProfileId = parseUserProfileUrl(url);
	if (userProfileId) {
		return () => {
			void UserProfileCommands.openLinkedUserProfile(userProfileId, context.guildId);
		};
	}
	const messageJumpTarget = parseMessageJumpLink(url);
	const channelJumpTarget = messageJumpTarget ? null : parseChannelJumpLink(url);
	const jumpTarget = messageJumpTarget ?? channelJumpTarget;
	const jumpChannel = jumpTarget ? (Channels.getChannel(jumpTarget.channelId) ?? null) : null;
	if (jumpTarget && !jumpChannel) {
		return () => {
			showChannelAccessDeniedModal(i18n);
		};
	}
	if (url === `${APP_PROTOCOL_PREFIX}dev` || url === `${APP_PROTOCOL_SCHEME}dev`) {
		return () => {
			if (DeveloperMode.isDeveloper) {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(DEVELOPER_TOOLS_DESCRIPTOR)}
							description={i18n._(DEVELOPER_TOOLS_ARE_AVAILABLE_FROM_THE_STAFF_BADGE_DESCRIPTOR)}
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
							data-flx="messaging.markdown.renderers.link-renderer.handle-click.confirm-modal--3"
						/>
					)),
				);
			}
		};
	}
	const settingsTarget = isAppProtocolUrl(url) ? parseUserSettingsDeepLink(url) : null;
	const appPage = settingsTarget ? null : parseAppPageLink(url);
	if (appPage) {
		return () => {
			navigateToAppPage(appPage);
		};
	}
	if (isAppProtocolUrl(url)) {
		return () => {
			if (handleDeepLinkUrl(url)) return;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(LINK_UNAVAILABLE_DESCRIPTOR)}
						description={i18n._(THIS_LINK_IS_NOT_AVAILABLE_FOR_YOUR_ACCOUNT_DESCRIPTOR)}
						data-flx="messaging.markdown.renderers.link-renderer.handle-click.confirm-modal--4"
					/>
				)),
			);
		};
	}
	if (parseOAuthAuthorizeModalUrl(url)) {
		return () => {
			if (!openOAuthAuthorizeModalFromUrl(url)) {
				void openExternalUrl(url);
			}
		};
	}
	let parsed: URL;
	try {
		parsed = new URL(url);
	} catch {
		return null;
	}
	const inviteCode = InviteUtils.findInviteUrl(url);
	if (inviteCode) {
		return () => {
			void InviteCommands.openAcceptModal(inviteCode, currentInstanceTarget());
		};
	}
	const themeCode = ThemeUtils.findThemeUrl(url);
	if (themeCode) {
		return () => {
			ThemeCommands.openAcceptModal(themeCode, i18n, RuntimeConfig.getSnapshot());
		};
	}
	if (!isInternalChannelHost(parsed.host) || !parsed.pathname.startsWith('/channels/')) {
		return null;
	}
	if (messageJumpTarget && jumpChannel) {
		return () => {
			const navigateToTarget = () => {
				if (LinkChannelCommands.openLinkChannel(jumpChannel, {skipGate: true})) {
					return;
				}
				goToMessage(messageJumpTarget.channelId, messageJumpTarget.messageId, {
					returnToMessageId: context.returnToMessageId,
					returnChannelId: context.returnChannelId,
				});
			};
			if (promptForJumpGate(jumpChannel, messageJumpTarget.scope, navigateToTarget)) {
				return;
			}
			navigateToTarget();
		};
	}
	if (channelJumpTarget) {
		return () => {
			const navigateToTarget = () => {
				if (jumpChannel && LinkChannelCommands.openLinkChannel(jumpChannel, {skipGate: true})) {
					return;
				}
				NavigationCommands.selectChannel(
					channelJumpTarget.scope === ME ? undefined : channelJumpTarget.scope,
					channelJumpTarget.channelId,
				);
			};
			if (jumpChannel && promptForJumpGate(jumpChannel, channelJumpTarget.scope, navigateToTarget)) {
				return;
			}
			navigateToTarget();
		};
	}
	if (parseChannelUrl(url)) {
		return () => {
			NavigationCommands.deselectGuild();
		};
	}
	return null;
}

export function openInAppOrExternalUrl(url: string, context: InAppLinkContext): void {
	const openInApp = getInAppLinkAction(url, context);
	if (openInApp) {
		openInApp();
		return;
	}
	openExternalUrlWithWarning(url);
}
