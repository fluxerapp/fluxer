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
import {ArrowSquareOutIcon} from '@phosphor-icons/react';
import type React from 'react';
import {useEffect, useState} from 'react';
import {Routes} from '~/Routes';
import {checkDesktopAvailable, navigateInDesktop} from '~/utils/DesktopRpcClient';
import {isDesktop} from '~/utils/NativeUtils';
import {Button} from '../uikit/Button/Button';
import styles from './DesktopDeepLinkPrompt.module.css';

interface DesktopDeepLinkPromptProps {
	code: string;
	kind: 'invite' | 'gift' | 'theme';
	preferLogin?: boolean;
}

export const DesktopDeepLinkPrompt: React.FC<DesktopDeepLinkPromptProps> = ({code, kind, preferLogin = false}) => {
	const [isLoading, setIsLoading] = useState(false);
	const [desktopAvailable, setDesktopAvailable] = useState<boolean | null>(null);
	const [error, setError] = useState<string | null>(null);

	useEffect(() => {
		if (isDesktop()) return;

		let cancelled = false;
		checkDesktopAvailable().then(({available}) => {
			if (!cancelled) {
				setDesktopAvailable(available);
			}
		});
		return () => {
			cancelled = true;
		};
	}, []);

	if (isDesktop()) return null;

	if (desktopAvailable !== true) return null;

	const getPath = (): string => {
		switch (kind) {
			case 'invite':
				return preferLogin ? Routes.inviteLogin(code) : Routes.inviteRegister(code);
			case 'gift':
				return preferLogin ? Routes.giftLogin(code) : Routes.giftRegister(code);
			case 'theme':
				return preferLogin ? Routes.themeLogin(code) : Routes.themeRegister(code);
		}
	};

	const path = getPath();

	const handleOpen = async () => {
		setIsLoading(true);
		setError(null);

		const result = await navigateInDesktop(path);

		setIsLoading(false);

		if (!result.success) {
			setError(result.error ?? 'Failed to open in desktop app');
		}
	};

	return (
		<div className={styles.banner}>
			<div className={styles.copy}>
				<p className={styles.title}>
					<Trans>Open in Fluxer for desktop</Trans>
				</p>
				{error ? (
					<p className={styles.notInstalled}>{error}</p>
				) : (
					<p className={styles.body}>
						<Trans>Jump straight to the app to continue.</Trans>
					</p>
				)}
			</div>
			<Button variant="primary" onClick={handleOpen} className={styles.cta} submitting={isLoading}>
				<ArrowSquareOutIcon size={18} weight="fill" />
				<span>
					<Trans>Open Fluxer</Trans>
				</span>
			</Button>
		</div>
	);
};
