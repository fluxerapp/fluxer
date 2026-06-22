// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import LocalRpcPresence from '@app/features/presence/state/LocalRpcPresence';
import Presence from '@app/features/presence/state/Presence';
import {getRpcActivitySubtitle, getRpcActivityVerb} from '@app/features/user/components/profile/RpcActivityDisplay';
import styles from '@app/features/user/components/profile/RpcActivitySection.module.css';
import type {UserActivity} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface RpcActivitySectionProps {
	userId: string;
	showAllActivities?: boolean;
	'data-flx'?: string;
}

interface RpcActivityCardProps {
	activity: UserActivity;
	'data-flx'?: string;
}

const RpcActivityCard: React.FC<RpcActivityCardProps> = ({activity, 'data-flx': dataFlx}) => {
	const subtitle = getRpcActivitySubtitle(activity);
	return (
		<div className={styles.card} data-flx={dataFlx ?? 'user.profile.rpc-activity-section.card'}>
			<span className={styles.activityLabel} data-flx="user.profile.rpc-activity-section.activity-label">
				{getRpcActivityVerb(activity)}
			</span>
			<span className={styles.activityName} data-flx="user.profile.rpc-activity-section.activity-name">
				{activity.name}
			</span>
			{subtitle ? (
				<span className={styles.activitySubtitle} data-flx="user.profile.rpc-activity-section.activity-subtitle">
					{subtitle}
				</span>
			) : null}
		</div>
	);
};

export const RpcActivitySection: React.FC<RpcActivitySectionProps> = observer(
	({userId, showAllActivities = false, 'data-flx': dataFlx}) => {
		Presence.presenceVersion;
		LocalRpcPresence.activityVersion;

		const activities =
			Authentication.currentUserId === userId
				? LocalRpcPresence.activity
					? [LocalRpcPresence.activity]
					: []
				: Presence.getActivities(userId);
		const visibleActivities = activities.filter((activity) => activity.name.trim());
		const renderedActivities = showAllActivities ? visibleActivities : visibleActivities.slice(0, 1);

		if (!renderedActivities.length) {
			return null;
		}

		if (showAllActivities) {
			return (
				<div className={styles.section} data-flx={dataFlx ?? 'user.profile.rpc-activity-section.section'}>
					<div className={styles.activityGrid} data-flx="user.profile.rpc-activity-section.activity-grid">
						{renderedActivities.map((activity) => (
							<RpcActivityCard
								key={`${activity.application_id ?? activity.name}:${activity.type}`}
								activity={activity}
								data-flx="user.profile.rpc-activity-section.rpc-activity-card"
							/>
						))}
					</div>
				</div>
			);
		}

		return (
			<div className={styles.section} data-flx={dataFlx ?? 'user.profile.rpc-activity-section.section--2'}>
				<RpcActivityCard
					activity={renderedActivities[0]}
					data-flx="user.profile.rpc-activity-section.rpc-activity-card--2"
				/>
			</div>
		);
	},
);
