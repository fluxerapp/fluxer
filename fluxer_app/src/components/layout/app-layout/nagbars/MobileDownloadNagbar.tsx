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
import {AndroidLogoIcon, AppleLogoIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import * as NagbarActionCreators from '~/actions/NagbarActionCreators';
import {Nagbar} from '~/components/layout/Nagbar';
import {NagbarButton} from '~/components/layout/NagbarButton';
import {NagbarContent} from '~/components/layout/NagbarContent';
import {openExternalUrl} from '~/utils/NativeUtils';
import styles from './MobileDownloadNagbar.module.css';

export const MobileDownloadNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const handleDownload = () => {
		openExternalUrl('https://fluxer.app/download#mobile');
	};

	const handleDismiss = () => {
		NagbarActionCreators.dismissNagbar('mobileDownloadDismissed');
	};

	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor="var(--brand-primary)"
			textColor="var(--text-on-brand-primary)"
			dismissible
			onDismiss={handleDismiss}
		>
			<NagbarContent
				isMobile={isMobile}
				message={
					<Trans>
						Get Fluxer on mobile to receive notifications on the go and stay connected with your friends anytime.
					</Trans>
				}
				actions={
					<>
						{isMobile && (
							<NagbarButton isMobile={isMobile} onClick={handleDismiss}>
								<Trans>Dismiss</Trans>
							</NagbarButton>
						)}
						<span className={styles.platformIcons}>
							<AppleLogoIcon weight="fill" className={styles.platformIcon} />
							<AndroidLogoIcon weight="fill" className={styles.platformIcon} />
						</span>
						<NagbarButton isMobile={isMobile} onClick={handleDownload}>
							<Trans>Download</Trans>
						</NagbarButton>
					</>
				}
			/>
		</Nagbar>
	);
});
