// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {AudioLevelMeter} from '@app/features/user/components/modals/tabs/components/AudioLevelMeter';
import styles from '@app/features/user/components/modals/tabs/components/MicTestSection.module.css';
import type {MicTestSettings} from '@app/features/user/components/modals/tabs/hooks/useMicTest';
import {useMicTest} from '@app/features/user/components/modals/tabs/hooks/useMicTest';
import {PlayIcon, StopIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface MicTestSectionProps {
	settings: MicTestSettings;
}

export const MicTestSection: React.FC<MicTestSectionProps> = observer(({settings}) => {
	const {isTesting, isStarting, level, start, stop} = useMicTest(settings);
	return (
		<div className={styles.bar} data-active={isTesting ? 'true' : 'false'} data-flx="user.mic-test-section.bar">
			<div className={styles.meter} data-flx="user.mic-test-section.meter">
				<AudioLevelMeter level={isTesting ? level : 0} data-flx="user.mic-test-section.audio-level-meter" />
			</div>

			<Button
				variant={isTesting ? 'secondary' : 'primary'}
				className={styles.actionButton}
				onClick={isTesting ? stop : start}
				submitting={isStarting}
				square={true}
				icon={
					isTesting ? (
						<StopIcon size={remFromPx(16)} weight="fill" data-flx="user.mic-test-section.stop-icon" />
					) : (
						<PlayIcon size={remFromPx(16)} weight="fill" data-flx="user.mic-test-section.start-icon" />
					)
				}
				aria-label={isTesting ? 'Stop mic test' : 'Start mic test'}
				data-flx="user.mic-test-section.action-button"
			/>
		</div>
	);
});
