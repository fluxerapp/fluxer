/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans} from '@lingui/react/macro';
import React from 'react';
import {FluxerIcon} from '~/components/icons/FluxerIcon';
import {Button} from '~/components/uikit/Button/Button';
import AppStorage from '~/lib/AppStorage';
import styles from './ErrorFallback.module.css';

interface BootstrapErrorScreenProps {
	error?: Error;
}

export const BootstrapErrorScreen: React.FC<BootstrapErrorScreenProps> = ({error}) => {
	const handleRetry = React.useCallback(() => {
		window.location.reload();
	}, []);

	const handleReset = React.useCallback(() => {
		AppStorage.clear();
		window.location.reload();
	}, []);

	return (
		<div className={styles.errorFallbackContainer}>
			<FluxerIcon className={styles.errorFallbackIcon} />
			<div className={styles.errorFallbackContent}>
				<h1 className={styles.errorFallbackTitle}>
					<Trans>Failed to Start</Trans>
				</h1>
				<p className={styles.errorFallbackDescription}>
					<Trans>Fluxer failed to start properly. This could be due to corrupted data or a temporary issue.</Trans>
				</p>
				{error && (
					<p className={styles.errorFallbackDescription} style={{fontSize: '0.875rem', opacity: 0.8}}>
						{error.message}
					</p>
				)}
				<p className={styles.errorFallbackDescription}>
					<Trans>
						Check our{' '}
						<a href="https://bsky.app/profile/fluxer.app" target="_blank" rel="noopener noreferrer">
							Bluesky (@fluxer.app)
						</a>{' '}
						for status updates.
					</Trans>
				</p>
			</div>
			<div className={styles.errorFallbackActions}>
				<Button onClick={handleRetry}>
					<Trans>Try Again</Trans>
				</Button>
				<Button onClick={handleReset} variant="danger-primary">
					<Trans>Reset App Data</Trans>
				</Button>
			</div>
		</div>
	);
};
