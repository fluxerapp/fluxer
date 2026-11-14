// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import {switchToSavedAccount} from '@app/features/auth/utils/AccountSwitcherModalUtils';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import {goToMessage, parseMessagePath} from '@app/features/messaging/utils/MessageNavigator';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {handleDeepLinkUrl, isAppDeepLink} from '@app/features/navigation/utils/DeepLinkUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import type {QuickSwitcherExecutableResult} from '@app/features/search/state/QuickSwitcherTypes';
import {QUICK_SWITCHER_OVERLAY_ID} from '@app/features/search/state/QuickSwitcherTypes';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {FAVORITES_GUILD_ID, ME} from '@fluxer/constants/src/AppConstants';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';

export function hide(): void {
	QuickSwitcher.hide();
}

export function search(query: string): void {
	QuickSwitcher.search(query);
}

export function select(selectedIndex: number): void {
	QuickSwitcher.select(selectedIndex);
}

export function moveSelection(direction: 'up' | 'down'): void {
	const nextIndex = QuickSwitcher.findNextSelectableIndex(direction);
	select(nextIndex);
}

export async function confirmSelection(): Promise<void> {
	const result = QuickSwitcher.getSelectedResult();
	if (!result) return;
	await switchTo(result);
}

export async function switchToOtherAccount(accountKey: string, redirectAfterSwitch?: string): Promise<void> {
	await switchToSavedAccount({
		accountKey,
		redirectAfterSwitch,
		onSelectCurrent: redirectAfterSwitch === undefined ? null : () => RouterUtils.transitionTo(redirectAfterSwitch),
	});
}

export async function switchTo(result: QuickSwitcherExecutableResult): Promise<void> {
	try {
		switch (result.type) {
			case QuickSwitcherResultTypes.ACCOUNT: {
				void switchToOtherAccount(result.accountKey);
				break;
			}
			case QuickSwitcherResultTypes.ACCOUNT_CHANNEL: {
				void switchToOtherAccount(result.accountKey, Routes.guildChannel(result.guildId ?? ME, result.channelId));
				break;
			}
			case QuickSwitcherResultTypes.USER: {
				const channelId = result.dmChannelId ?? (await PrivateChannelCommands.ensureDMChannel(result.user.id));
				NavigationCommands.selectChannel(ME, channelId);
				focusChannelTextareaAfterNavigation(channelId);
				break;
			}
			case QuickSwitcherResultTypes.GROUP_DM: {
				NavigationCommands.selectChannel(ME, result.channel.id);
				focusChannelTextareaAfterNavigation(result.channel.id);
				break;
			}
			case QuickSwitcherResultTypes.TEXT_CHANNEL: {
				if (result.guild) {
					NavigationCommands.selectChannel(result.guild.id, result.channel.id);
				} else {
					NavigationCommands.selectChannel(ME, result.channel.id);
				}
				focusChannelTextareaAfterNavigation(result.channel.id);
				break;
			}
			case QuickSwitcherResultTypes.VOICE_CHANNEL: {
				if (result.guild) {
					NavigationCommands.selectChannel(result.guild.id, result.channel.id);
				}
				break;
			}
			case QuickSwitcherResultTypes.GUILD: {
				const channelId = SelectedChannel.selectedChannelIds.get(result.guild.id);
				NavigationCommands.selectGuild(result.guild.id, channelId);
				break;
			}
			case QuickSwitcherResultTypes.VIRTUAL_GUILD: {
				if (result.virtualGuildType === 'favorites') {
					const validChannelId = SelectedChannel.getValidatedFavoritesChannel();
					NavigationCommands.selectGuild(FAVORITES_GUILD_ID, validChannelId ?? undefined);
				} else if (result.virtualGuildType === 'home') {
					const dmChannelId = SelectedChannel.selectedChannelIds.get(ME);
					NavigationCommands.selectGuild(ME, dmChannelId);
				}
				break;
			}
			case QuickSwitcherResultTypes.SETTINGS: {
				const initialTab = result.settingsTab.type;
				const initialSubtab = result.settingsSubtab?.type;
				ModalCommands.push(
					modal(
						() => (
							<UserSettingsModal
								initialTab={initialTab}
								initialSubtab={initialSubtab}
								data-flx="search.quick-switcher-commands.switch-to.user-settings-modal"
							/>
						),
						'user-settings',
					),
				);
				break;
			}
			case QuickSwitcherResultTypes.LINK: {
				if (isAppDeepLink(result.path)) {
					handleDeepLinkUrl(result.path);
					break;
				}
				const parsed = parseMessagePath(result.path);
				if (parsed) {
					const viewContext = result.path.startsWith(Routes.favoritesChannel(parsed.channelId))
						? 'favorites'
						: undefined;
					goToMessage(parsed.channelId, parsed.messageId, {viewContext});
					focusChannelTextareaAfterNavigation(parsed.channelId);
				} else {
					RouterUtils.transitionTo(result.path);
				}
				break;
			}
			default:
				break;
		}
	} finally {
		hide();
	}
}

export function getModalKey(): string {
	return QUICK_SWITCHER_OVERLAY_ID;
}
