// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceActivityCard} from '@app/features/user/components/profile/VoiceActivityCard';
import styles from '@app/features/user/components/profile/VoiceActivitySection.module.css';
import {useUserVoiceActivityAggregates} from '@app/features/voice/hooks/useUserVoiceActivities';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const AND_OTHER_CALLS_DESCRIPTOR = msg({
	message: '{additionalCount, plural, one {And # other call} other {And # other calls}}',
	comment: 'Label in the user settings voice activity section. Preserve {additionalCount}; it is inserted by code.',
});

interface VoiceActivitySectionProps {
	userId: string;
	onNavigate?: () => void;
	showAllActivities?: boolean;
}

export const VoiceActivitySection: React.FC<VoiceActivitySectionProps> = observer(
	({userId, onNavigate, showAllActivities = false}) => {
		const {i18n} = useLingui();
		const activityAggregates = useUserVoiceActivityAggregates(userId);
		const primaryActivity = activityAggregates[0]?.primaryActivity;
		const aggregatedActivities = useMemo(
			() => activityAggregates.map((aggregate) => aggregate.primaryActivity),
			[activityAggregates],
		);
		const additionalCount = Math.max(0, aggregatedActivities.length - 1);
		const additionalCallsLabel = useMemo(
			() => i18n._(AND_OTHER_CALLS_DESCRIPTOR, {additionalCount}),
			[additionalCount, i18n.locale],
		);
		if (!primaryActivity) {
			return null;
		}
		if (showAllActivities) {
			return (
				<div className={styles.section} data-flx="user.profile.voice-activity-section.section">
					<div className={styles.allCallsGrid} data-flx="user.profile.voice-activity-section.all-calls-grid">
						{aggregatedActivities.map((activity) => (
							<div
								key={`${activity.guildId ?? 'dm'}:${activity.channelId}`}
								className={styles.gridItem}
								data-flx="user.profile.voice-activity-section.grid-item"
							>
								<VoiceActivityCard
									activity={activity}
									onNavigate={onNavigate}
									data-flx="user.profile.voice-activity-section.voice-activity-card"
								/>
							</div>
						))}
					</div>
				</div>
			);
		}
		return (
			<div className={styles.section} data-flx="user.profile.voice-activity-section.section--2">
				<VoiceActivityCard
					activity={primaryActivity}
					onNavigate={onNavigate}
					data-flx="user.profile.voice-activity-section.voice-activity-card--2"
				/>
				{additionalCount > 0 ? (
					<span className={styles.moreCallsText} data-flx="user.profile.voice-activity-section.more-calls-text">
						{additionalCallsLabel}
					</span>
				) : null}
			</div>
		);
	},
);
