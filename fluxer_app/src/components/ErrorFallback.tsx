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
import {observer} from 'mobx-react-lite';
import React from 'react';
import errorFallbackStyles from '~/components/ErrorFallback.module.css';
import {FluxerIcon} from '~/components/icons/FluxerIcon';
import {NativeTitlebar} from '~/components/layout/NativeTitlebar';
import {Button} from '~/components/uikit/Button/Button';
import {useNativePlatform} from '~/hooks/useNativePlatform';
import AppStorage from '~/lib/AppStorage';
import {ensureLatestAssets} from '~/lib/versioning';

interface ErrorFallbackProps {
	error?: Error;
	reset?: () => void;
}

export const ErrorFallback: React.FC<ErrorFallbackProps> = observer(() => {
	const {platform, isNative, isMacOS} = useNativePlatform();
	const [updateAvailable, setUpdateAvailable] = React.useState(false);
	const [isUpdating, setIsUpdating] = React.useState(false);
	const [checkingForUpdates, setCheckingForUpdates] = React.useState(true);

	React.useEffect(() => {
		let isMounted = true;

		const run = async () => {
			try {
				const {updateFound} = await ensureLatestAssets({force: true});
				if (isMounted) {
					setUpdateAvailable(updateFound);
				}
			} catch (error) {
				console.error('[ErrorFallback] Failed to check for updates:', error);
			} finally {
				if (isMounted) {
					setCheckingForUpdates(false);
				}
			}
		};

		void run();

		return () => {
			isMounted = false;
		};
	}, []);

	const handleUpdate = React.useCallback(async () => {
		setIsUpdating(true);
		try {
			const {updateFound} = await ensureLatestAssets({force: true});
			if (!updateFound) {
				setIsUpdating(false);
				window.location.reload();
			}
		} catch (error) {
			console.error('[ErrorFallback] Failed to apply update:', error);
			setIsUpdating(false);
		}
	}, []);

	return (
		<div className={errorFallbackStyles.errorFallbackContainer}>
			{isNative && !isMacOS && <NativeTitlebar platform={platform} />}
			<FluxerIcon className={errorFallbackStyles.errorFallbackIcon} />
			<div className={errorFallbackStyles.errorFallbackContent}>
				<h1 className={errorFallbackStyles.errorFallbackTitle}>
					<Trans>Whoa, this is heavy.</Trans>
				</h1>
				<p className={errorFallbackStyles.errorFallbackDescription}>
					{checkingForUpdates ? (
						<Trans>The app has crashed. Checking for updates that might fix this issue...</Trans>
					) : updateAvailable ? (
						<Trans>Something went wrong and the app crashed. An update is available that may fix this issue.</Trans>
					) : (
						<Trans>Something went wrong and the app crashed. Try reloading or resetting the app.</Trans>
					)}
				</p>
			</div>
			<div className={errorFallbackStyles.errorFallbackActions}>
				<Button
					onClick={updateAvailable ? handleUpdate : () => location.reload()}
					disabled={checkingForUpdates || isUpdating}
				>
					{isUpdating ? (
						<Trans>Updating...</Trans>
					) : checkingForUpdates || updateAvailable ? (
						<Trans>Update app</Trans>
					) : (
						<Trans>Reload app</Trans>
					)}
				</Button>
				<Button
					onClick={() => {
						AppStorage.clear();
						location.reload();
					}}
					variant="danger-primary"
					disabled={checkingForUpdates}
				>
					<Trans>Reset app data</Trans>
				</Button>
			</div>
		</div>
	);
});
