// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/expressions/components/FavoritesWelcomeSection.module.css';
import * as FavoritesCommands from '@app/features/messaging/commands/FavoritesCommands';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Button} from '@app/features/ui/button/Button';
import {Trans, useLingui} from '@lingui/react/macro';
import {StarIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

export const FavoritesWelcomeSection = observer(() => {
	const {i18n} = useLingui();
	const handleDisableFavorites = useCallback(() => {
		FavoritesCommands.confirmHideFavorites(() => {
			RouterUtils.transitionTo(Routes.ME);
		}, i18n);
	}, [i18n]);
	return (
		<div className={styles.welcomeSection} data-flx="expressions.favorites-welcome-section.welcome-section">
			<div className={styles.iconSection} data-flx="expressions.favorites-welcome-section.icon-section">
				<div className={styles.iconWrapper} data-flx="expressions.favorites-welcome-section.icon-wrapper">
					<StarIcon className={styles.icon} weight="fill" data-flx="expressions.favorites-welcome-section.icon" />
				</div>
			</div>
			<div className={styles.contentSection} data-flx="expressions.favorites-welcome-section.content-section">
				<h1 className={styles.heading} data-flx="expressions.favorites-welcome-section.heading">
					<Trans>Welcome to favorites</Trans>
				</h1>
				<p className={styles.description} data-flx="expressions.favorites-welcome-section.description">
					{RuntimeConfig.directMessagesDisabled ? (
						<Trans>
							Your personal space for quick access to the channels you love. Press the star on any channel to add it
							here.
						</Trans>
					) : (
						<Trans>
							Your personal space for quick access to channels, DMs, and groups you love. Press the star on any channel
							to add it here.
						</Trans>
					)}
				</p>
				<p className={styles.tip} data-flx="expressions.favorites-welcome-section.tip">
					<Trans>Not for you? Turn it off anytime.</Trans>
				</p>
				<div className={styles.actionSection} data-flx="expressions.favorites-welcome-section.action-section">
					<Button
						variant="secondary"
						onClick={handleDisableFavorites}
						data-flx="expressions.favorites-welcome-section.button.disable-favorites"
					>
						<Trans>Disable favorites</Trans>
					</Button>
				</div>
			</div>
		</div>
	);
});
