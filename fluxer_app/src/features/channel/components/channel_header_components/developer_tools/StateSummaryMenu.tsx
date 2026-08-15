// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ActiveOverrideMenuGroup,
	BackendPremiumOverrideMenuGroup,
	BackendPremiumPerksDisabledMenuGroup,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsMenuComponents';
import {
	ACTIVE_DESCRIPTOR,
	humanizeDeveloperStateKey,
	nonEmptyText,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsShared';
import {getNagbarControls} from '@app/features/devtools/components/NagbarControls';
import * as PremiumCommands from '@app/features/premium/commands/PremiumCommands';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import Nagbar from '@app/features/ui/state/Nagbar';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import Users from '@app/features/user/state/Users';
import {msg, plural} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {BellRingingIcon, TrashIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {getActiveDeveloperOptionEntries} from './DeveloperOptionLabels';
import {setNagbarUseActual} from './NagbarControls';
import {resetAllDeveloperOptions} from './ResetOptions';

const NO_ACTIVE_OVERRIDES_DESCRIPTOR = msg({
	message: 'No active overrides',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REVIEW_ACTIVE_OVERRIDES_DESCRIPTOR = msg({
	message: 'Review active overrides',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const BACKEND_PREMIUM_OVERRIDE_DESCRIPTOR = msg({
	message: 'Backend premium override',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const BACKEND_PREMIUM_OVERRIDE_ENABLED_DESCRIPTOR = msg({
	message: 'Backend premium override enabled',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const BACKEND_PREMIUM_PERKS_DISABLED_DESCRIPTOR = msg({
	message: 'Backend premium perks disabled',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const PREMIUM_PERKS_DISABLED_DESCRIPTOR = msg({
	message: 'Premium perks disabled',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const DeveloperStateSummaryMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const developerEntries = getActiveDeveloperOptionEntries(i18n);
	const nagbarEntries = getNagbarControls()
		.filter((control) => !(control.useActualDisabled?.(Nagbar) ?? false))
		.map((control) => ({
			key: control.key,
			label: nonEmptyText(i18n._(control.label), humanizeDeveloperStateKey(control.key)),
			value: nonEmptyText(i18n._(control.status(Nagbar)), i18n._(ACTIVE_DESCRIPTOR)),
			reset: () => setNagbarUseActual(control),
		}));
	const backendPremiumOverrideActive = user?.premiumEnabledOverride === true;
	const backendPremiumPerksDisabledActive = user?.premiumPerksDisabled === true;
	const activeCount =
		developerEntries.length +
		nagbarEntries.length +
		(backendPremiumOverrideActive ? 1 : 0) +
		(backendPremiumPerksDisabledActive ? 1 : 0);
	const activeHint =
		activeCount === 0
			? i18n._(NO_ACTIVE_OVERRIDES_DESCRIPTOR)
			: plural({count: activeCount}, {one: '# active override', other: '# active overrides'});
	const handleClearAll = () => {
		resetAllDeveloperOptions();
		NagbarCommands.resetAllNagbars();
		if (backendPremiumOverrideActive) {
			void UserCommands.update({premium_enabled_override: false});
		}
		if (backendPremiumPerksDisabledActive) {
			void PremiumCommands.setPremiumPerksDisabled(false);
		}
	};
	return (
		<>
			<MenuItem
				disabled
				icon={
					<WarningCircleIcon
						size={remFromPx(16)}
						weight="fill"
						data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.warning-circle-icon"
					/>
				}
				hint={activeHint}
				data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.menu-item"
			>
				<Trans>Developer state</Trans>
			</MenuItem>
			<MenuItemSubmenu
				label={i18n._(REVIEW_ACTIVE_OVERRIDES_DESCRIPTOR)}
				disabled={activeCount === 0}
				hint={activeHint}
				render={() => (
					<>
						<ActiveOverrideMenuGroup
							entries={developerEntries}
							icon={() => (
								<TrashIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.trash-icon"
								/>
							)}
							data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.active-override-menu-group"
						/>
						<ActiveOverrideMenuGroup
							entries={nagbarEntries}
							icon={() => (
								<BellRingingIcon
									size={remFromPx(16)}
									weight="fill"
									data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.bell-ringing-icon"
								/>
							)}
							data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.active-override-menu-group--2"
						/>
						{backendPremiumOverrideActive && (
							<BackendPremiumOverrideMenuGroup
								label={i18n._(BACKEND_PREMIUM_OVERRIDE_DESCRIPTOR)}
								hint={i18n._(BACKEND_PREMIUM_OVERRIDE_ENABLED_DESCRIPTOR)}
								data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.backend-premium-override-menu-group"
							/>
						)}
						{backendPremiumPerksDisabledActive && (
							<BackendPremiumPerksDisabledMenuGroup
								label={i18n._(BACKEND_PREMIUM_PERKS_DISABLED_DESCRIPTOR)}
								hint={i18n._(PREMIUM_PERKS_DISABLED_DESCRIPTOR)}
								data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.backend-premium-perks-disabled-menu-group"
							/>
						)}
					</>
				)}
				data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.menu-item-submenu"
			/>
			<MenuItem
				icon={
					<TrashIcon
						size={remFromPx(16)}
						weight="bold"
						data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.trash-icon--2"
					/>
				}
				danger
				disabled={activeCount === 0}
				onClick={handleClearAll}
				closeOnSelect={false}
				data-flx="channel.channel-header-components.developer-tools-context-menu.developer-state-summary-menu.menu-item.clear-all"
			>
				<Trans>Clear all active overrides</Trans>
			</MenuItem>
		</>
	);
});
