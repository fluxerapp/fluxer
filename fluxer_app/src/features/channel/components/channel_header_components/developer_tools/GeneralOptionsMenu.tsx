// SPDX-License-Identifier: AGPL-3.0-or-later

import {translateDescriptor} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsShared';
import {getToggleGroups, type ToggleGroup} from '@app/features/devtools/components/DeveloperOptionsToggleGroups';
import type {DeveloperOptionsState} from '@app/features/devtools/state/DeveloperOptions';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {updateOption} from './ResetOptions';

const MENTION_CONTROLS_DESCRIPTOR = msg({
	message: 'Mention controls',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const ToggleGroupSubmenu: React.FC<{group: ToggleGroup}> = observer(({group}) => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={translateDescriptor(i18n, group.title)}
			render={() => (
				<>
					{group.items.map(({key, label, description}) => {
						const translatedLabel = translateDescriptor(i18n, label);
						return (
							<CheckboxItem
								key={String(key)}
								label={translatedLabel}
								checked={Boolean(DeveloperOptions[key])}
								onCheckedChange={(checked) => {
									updateOption(key, checked as DeveloperOptionsState[typeof key]);
									if (key === 'selfHostedModeOverride') {
										window.location.reload();
									}
								}}
								data-flx="channel.channel-header-components.developer-tools-context-menu.toggle-group-submenu.checkbox-item"
							>
								{description ? (
									// biome-ignore lint/a11y/useAriaPropsSupportedByRole: project policy forbids the native title attribute, so aria-label carries the description on the developer-options row
									<span
										aria-label={translateDescriptor(i18n, description)}
										data-flx="channel.channel-header-components.developer-tools-context-menu.toggle-group-submenu.span"
									>
										{translatedLabel}
									</span>
								) : (
									translatedLabel
								)}
							</CheckboxItem>
						);
					})}
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.toggle-group-submenu.menu-item-submenu"
		/>
	);
});
export const GeneralDeveloperOptionsMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	const toggleGroups = getToggleGroups();
	const currentUser = Users.currentUser;
	const canConfigureMentionSuppression = currentUser?.isStaff() ?? false;
	const suppressUnprivilegedSelfMentions = UserSettings.getSuppressUnprivilegedSelfMentions();
	return (
		<>
			{toggleGroups.map((group, index) => (
				<ToggleGroupSubmenu
					key={group.title.id ?? index}
					group={group}
					data-flx="channel.channel-header-components.developer-tools-context-menu.general-developer-options-menu.toggle-group-submenu"
				/>
			))}
			{canConfigureMentionSuppression && (
				<MenuItemSubmenu
					label={i18n._(MENTION_CONTROLS_DESCRIPTOR)}
					render={() => (
						<CheckboxItem
							checked={suppressUnprivilegedSelfMentions}
							onCheckedChange={(checked) => {
								void UserSettingsCommands.update({suppressUnprivilegedSelfMentions: checked});
							}}
							data-flx="channel.channel-header-components.developer-tools-context-menu.general-developer-options-menu.checkbox-item"
						>
							<Trans>Suppress unprivileged self-mentions</Trans>
						</CheckboxItem>
					)}
					data-flx="channel.channel-header-components.developer-tools-context-menu.general-developer-options-menu.menu-item-submenu"
				/>
			)}
		</>
	);
});
