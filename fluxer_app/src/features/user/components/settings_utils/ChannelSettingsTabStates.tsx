// SPDX-License-Identifier: AGPL-3.0-or-later

import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {LoadableErrorProps} from '@app/features/platform/components/loadable/LoadableComponent';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import styles from '@app/features/user/components/settings_utils/ChannelSettingsTabStates.module.css';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';

export function ChannelSettingsTabPending(): React.JSX.Element {
	return (
		<div className={styles.state} data-flx="user.channel-settings-tab-states.pending">
			<Spinner data-flx="user.channel-settings-tab-states.pending.spinner" />
		</div>
	);
}

export function ChannelSettingsTabUnavailable({retry}: LoadableErrorProps): React.JSX.Element {
	const {i18n} = useLingui();
	return (
		<div className={styles.state} role="alert" data-flx="user.channel-settings-tab-states.unavailable">
			<p className={styles.message} data-flx="user.channel-settings-tab-states.unavailable.message">
				<Trans>Couldn't load this tab. Check your connection and try again.</Trans>
			</p>
			<Button small onClick={retry} data-flx="user.channel-settings-tab-states.unavailable.button.retry">
				{i18n._(TRY_AGAIN_DESCRIPTOR)}
			</Button>
		</div>
	);
}
