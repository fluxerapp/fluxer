// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import styles from '@app/features/streamer_mode/components/StreamerModeGate.module.css';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {EyeSlashIcon} from '@phosphor-icons/react';
import type React from 'react';

const STREAMER_MODE_ENABLED_DESCRIPTOR = msg({
	message: 'Streaming privacy on',
	comment: 'Banner label shown in settings while streaming privacy is enabled.',
});
const SENSITIVE_SETTINGS_HIDDEN_DESCRIPTOR = msg({
	message: 'Account and security details are hidden',
	comment: 'Title for a gate that hides account/security settings while streaming privacy is enabled.',
});
const TURN_OFF_STREAMER_MODE_TO_VIEW_DESCRIPTOR = msg({
	message: 'Turn off streaming privacy before viewing login or account security details.',
	comment: 'Description for a gate that hides sensitive settings while streaming privacy is enabled.',
});
const GO_TO_STREAMER_MODE_SETTINGS_DESCRIPTOR = msg({
	message: 'Open streaming privacy settings',
	comment: 'Button label that navigates to streaming privacy settings.',
});

export const StreamerModeGate: React.FC = () => {
	const {i18n} = useLingui();
	const goToStreamerModeSettings = () => {
		ComponentBus.dispatch('USER_SETTINGS_TAB_SELECT', {tab: 'appearance', section: 'streamer-mode'});
	};
	return (
		<div className={styles.screen} data-flx="streamer-mode.gate.screen">
			<div className={styles.panel} data-flx="streamer-mode.gate.panel">
				<div className={styles.privacyMark} aria-hidden="true" data-flx="streamer-mode.gate.privacy-mark">
					<EyeSlashIcon weight="duotone" className={styles.markIcon} data-flx="streamer-mode.gate.mark-icon" />
				</div>
				<div className={styles.statusPill} data-flx="streamer-mode.gate.status-pill">
					{i18n._(STREAMER_MODE_ENABLED_DESCRIPTOR)}
				</div>
				<h3 className={styles.headline} data-flx="streamer-mode.gate.headline">
					{i18n._(SENSITIVE_SETTINGS_HIDDEN_DESCRIPTOR)}
				</h3>
				<p className={styles.supportingText} data-flx="streamer-mode.gate.supporting-text">
					{i18n._(TURN_OFF_STREAMER_MODE_TO_VIEW_DESCRIPTOR)}
				</p>
				<Button
					variant="primary"
					fitContent
					onClick={goToStreamerModeSettings}
					data-flx="streamer-mode.gate.go-to-settings-button"
				>
					{i18n._(GO_TO_STREAMER_MODE_SETTINGS_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
};
