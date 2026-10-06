// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/VoiceChannelUserCount.module.css';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {observer} from 'mobx-react-lite';

interface VoiceChannelUserCountProps {
	currentUserCount: number;
	userLimit: number;
}

export const VoiceChannelUserCount = observer(function VoiceChannelUserCount({
	currentUserCount,
	userLimit,
}: VoiceChannelUserCountProps) {
	const formatter = getCachedNumberFormat(getCurrentLocale(), {minimumIntegerDigits: 2, useGrouping: false});
	return (
		<div className={styles.wrapper} data-flx="app.voice-channel-user-count.wrapper">
			<span className={styles.users} data-flx="app.voice-channel-user-count.users">
				{formatter.format(currentUserCount)}
			</span>
			<span className={styles.total} data-flx="app.voice-channel-user-count.total">
				{formatter.format(userLimit)}
			</span>
		</div>
	);
});
