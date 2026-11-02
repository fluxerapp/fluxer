// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsTabContainer} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {OperatorPassHoldingsSection} from '@app/features/premium/components/operator_pass/OperatorPassHoldingsSection';
import {OperatorPassLabsGreetingSetting} from '@app/features/user/components/modals/tabs/operator_pass_tab/OperatorPassLabsGreetingSetting';
import {OperatorPassWallSettings} from '@app/features/user/components/modals/tabs/operator_pass_tab/OperatorPassWallSettings';
import {observer} from 'mobx-react-lite';

const OperatorPassTab = observer(function OperatorPassTab() {
	return (
		<SettingsTabContainer data-flx="user.operator-pass-tab.settings-tab-container">
			<OperatorPassHoldingsSection data-flx="user.operator-pass-tab.operator-pass-holdings-section" />
			<OperatorPassWallSettings data-flx="user.operator-pass-tab.operator-pass-wall-settings" />
			<OperatorPassLabsGreetingSetting data-flx="user.operator-pass-tab.operator-pass-labs-greeting-setting" />
		</SettingsTabContainer>
	);
});

export default OperatorPassTab;
