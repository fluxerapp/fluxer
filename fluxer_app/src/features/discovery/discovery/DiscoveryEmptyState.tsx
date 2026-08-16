// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/discovery/discovery/DiscoveryEmptyState.module.css';
import Discovery from '@app/features/discovery/state/Discovery';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CompassIcon, type Icon, MagnifyingGlassIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const NO_COMMUNITIES_MATCH_DESCRIPTOR = msg({
	message: 'No communities match.',
	comment: 'Empty-state text in the discovery page.',
});
const NO_MATCHING_COMMUNITIES_HINT_DESCRIPTOR = msg({
	message: 'Try another search term, or pick a different category or language.',
	comment: 'Empty-state hint in Discovery shown under the heading when a search returns no communities.',
});
const NOTHING_TO_EXPLORE_DESCRIPTOR = msg({
	message: 'Nothing to explore yet',
	comment: 'Empty-state heading in Discovery shown when no communities are listed at all.',
});
const NOTHING_TO_EXPLORE_HINT_DESCRIPTOR = msg({
	message: 'No communities are listed right now. Check back a little later.',
	comment: 'Empty-state hint in Discovery shown under the heading when no communities are listed at all.',
});
const COULD_NOT_LOAD_COMMUNITIES_DESCRIPTOR = msg({
	message: "Couldn't load communities",
	comment: 'Empty-state heading in Discovery shown when the request for communities failed.',
});
const COULD_NOT_LOAD_COMMUNITIES_HINT_DESCRIPTOR = msg({
	message: 'Check your connection, then try again.',
	comment: 'Empty-state hint in Discovery shown under the heading when the request for communities failed.',
});

interface DiscoveryEmptyStateContent {
	readonly icon: Icon;
	readonly heading: string;
	readonly description: string;
}

interface DiscoveryEmptyStateProps {
	readonly onRetry: () => void;
}

export const DiscoveryEmptyState = observer(function DiscoveryEmptyState({onRetry}: DiscoveryEmptyStateProps) {
	const {i18n} = useLingui();
	const failed = Discovery.error;
	const resolveContent = (): DiscoveryEmptyStateContent => {
		if (failed) {
			return {
				icon: WarningCircleIcon,
				heading: i18n._(COULD_NOT_LOAD_COMMUNITIES_DESCRIPTOR),
				description: i18n._(COULD_NOT_LOAD_COMMUNITIES_HINT_DESCRIPTOR),
			};
		}
		if (Discovery.query.length > 0) {
			return {
				icon: MagnifyingGlassIcon,
				heading: i18n._(NO_COMMUNITIES_MATCH_DESCRIPTOR),
				description: i18n._(NO_MATCHING_COMMUNITIES_HINT_DESCRIPTOR),
			};
		}
		return {
			icon: CompassIcon,
			heading: i18n._(NOTHING_TO_EXPLORE_DESCRIPTOR),
			description: i18n._(NOTHING_TO_EXPLORE_HINT_DESCRIPTOR),
		};
	};
	const content = resolveContent();
	const EmptyStateIcon = content.icon;
	return (
		<div className={styles.container} data-flx="discovery.discovery.discovery-empty-state.container">
			<EmptyStateIcon
				className={styles.icon}
				weight="duotone"
				aria-hidden
				data-flx="discovery.discovery.discovery-empty-state.icon"
			/>
			<div className={styles.text} data-flx="discovery.discovery.discovery-empty-state.text">
				<h3 className={styles.heading} data-flx="discovery.discovery.discovery-empty-state.heading">
					{content.heading}
				</h3>
				<p className={styles.description} data-flx="discovery.discovery.discovery-empty-state.description">
					{content.description}
				</p>
			</div>
			{failed && (
				<Button small onClick={onRetry} data-flx="discovery.discovery.discovery-empty-state.button.retry">
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</Button>
			)}
		</div>
	);
});
