// SPDX-License-Identifier: AGPL-3.0-or-later

import NewDeviceMonitoring from '@app/features/auth/state/NewDeviceMonitoring';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {SHOW_NEW_DEVICE_ALERTS_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const SHOW_CONNECTION_VOLUME_CONTROLS_DESCRIPTOR = msg({
	message: 'Show connection volume controls',
	comment:
		'Short label for an advanced voice preference that shows per-device participant volume sliders in voice menus.',
});

export const NewDeviceAlertsControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SHOW_NEW_DEVICE_ALERTS_DESCRIPTOR)}
			value={!NewDeviceMonitoring.suppressAlerts}
			onChange={(value) => NewDeviceMonitoring.setSuppressAlerts(!value)}
			compact
			data-flx="user.advanced-settings-tab.switch.new-device-alerts"
		/>
	);
});

export const ConnectionVolumeControlsControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SHOW_CONNECTION_VOLUME_CONTROLS_DESCRIPTOR)}
			value={VoiceSettings.showConnectionVolumeControls}
			onChange={(value) => VoiceSettingsCommands.update({showConnectionVolumeControls: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.connection-volume-controls"
		/>
	);
});
