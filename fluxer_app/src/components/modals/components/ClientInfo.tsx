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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import Config from '~/Config';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import DeveloperModeStore from '~/stores/DeveloperModeStore';
import {getClientInfoSync} from '~/utils/ClientInfoUtils';
import * as DateUtils from '~/utils/DateUtils';
import styles from './ClientInfo.module.css';

export const ClientInfo = observer(() => {
	const {t, i18n} = useLingui();
	const clientInfo = getClientInfoSync();

	const buildShaShort = (Config.PUBLIC_BUILD_SHA ?? '').slice(0, 7);
	const buildNumber = Config.PUBLIC_BUILD_NUMBER;
	const desktopVersion = clientInfo.desktopVersion;
	const desktopChannel = clientInfo.desktopChannel;

	const browserName = clientInfo.browserName || 'Unknown';
	const browserVersion = clientInfo.browserVersion || '';
	const osName = clientInfo.osName || 'Unknown';
	const osVersion =
		clientInfo.osVersion && clientInfo.arch
			? `${clientInfo.osVersion} (${clientInfo.arch})`
			: clientInfo.osVersion || clientInfo.arch || '';

	const onClick = () => {
		let timestamp = '';
		if (Config.PUBLIC_BUILD_TIMESTAMP) {
			const date = new Date(Config.PUBLIC_BUILD_TIMESTAMP * 1000);
			const year = date.getUTCFullYear();
			const month = String(date.getUTCMonth() + 1).padStart(2, '0');
			const day = String(date.getUTCDate()).padStart(2, '0');
			const hours = String(date.getUTCHours()).padStart(2, '0');
			const minutes = String(date.getUTCMinutes()).padStart(2, '0');
			const seconds = String(date.getUTCSeconds()).padStart(2, '0');
			timestamp = `, ${year}-${month}-${day} ${hours}:${minutes}:${seconds} UTC`;
		}
		const justUnlocked = DeveloperModeStore.registerBuildTap();
		if (justUnlocked) {
			ToastActionCreators.success(t`You are now a developer!`);
		}

		const desktopInfo = desktopVersion && desktopChannel ? `, desktop ${desktopChannel} ${desktopVersion}` : '';
		const buildInfo = buildNumber ? `build ${buildNumber} (${buildShaShort})` : `(${buildShaShort})`;

		TextCopyActionCreators.copy(
			i18n,
			`${Config.PUBLIC_PROJECT_ENV} ${buildInfo}${timestamp}, ${browserName} ${browserVersion}, ${osName} ${osVersion}${desktopInfo}`,
		);
	};

	return (
		<Tooltip text={t`Click to copy`}>
			<FocusRing>
				<button type="button" onClick={onClick} className={styles.button}>
					<span>
						{Config.PUBLIC_PROJECT_ENV} {buildNumber ? `build ${buildNumber} (${buildShaShort})` : `(${buildShaShort})`}
					</span>
					{desktopVersion && (
						<span>
							Desktop {desktopChannel ?? 'stable'} {desktopVersion}
						</span>
					)}
					{Config.PUBLIC_BUILD_TIMESTAMP && (
						<span>
							<Trans>Deployed</Trans> {DateUtils.getShortRelativeDateString(Config.PUBLIC_BUILD_TIMESTAMP * 1000)}
						</span>
					)}
					<span>
						{browserName} {browserVersion}
					</span>
					<span>
						{osName} {osVersion}
					</span>
				</button>
			</FocusRing>
		</Tooltip>
	);
});
