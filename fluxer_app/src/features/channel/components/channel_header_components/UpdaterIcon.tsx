// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Updater from '@app/features/app/state/Updater';
import styles from '@app/features/channel/components/ChannelHeader.module.css';
import {Platform} from '@app/features/platform/types/Platform';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowClockwiseIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const UPDATE_AVAILABLE_DESCRIPTOR = msg({
	message: 'Update available',
	comment:
		'Tooltip and accessible label on the channel header updater icon in the desktop app. Clicking it downloads the update, then either reloads the window or, when the desktop app itself updates, closes it and opens the updater.',
});
const UPDATING_DESCRIPTOR = msg({
	message: 'Updating…',
	comment:
		'Tooltip and accessible label on the channel header updater icon in the desktop app while an update the user started is downloading. The app reloads by itself when it is done.',
});
const CHOOSE_LINUX_PACKAGE_DESCRIPTOR = msg({
	message: 'Desktop update {version} available. Choose a Linux package.',
	comment: 'Tooltip on the updater icon for a Linux desktop update that requires choosing a package format.',
});
const CHOOSE_LINUX_PACKAGE_2_DESCRIPTOR = msg({
	message: 'Desktop update available. Choose a Linux package.',
	comment:
		'Tooltip on the updater icon for a Linux desktop update that requires choosing a package format and no version is known.',
});
const CLICK_TO_RELOAD_AND_UPDATE_DESCRIPTOR = msg({
	message: 'Reload {productName} to use web update {version}',
	comment:
		'Tooltip on the channel header updater icon prompting a web reload to apply an in-place build update. productName is the app name.',
});
const CLICK_TO_RELOAD_AND_UPDATE_2_DESCRIPTOR = msg({
	message: 'Reload {productName} to use web update',
	comment:
		'Tooltip on the channel header updater icon prompting a web reload to apply an update of unknown version. productName is the app name.',
});

type UpdaterAffordance = 'desktop' | 'updating' | 'manual' | 'web' | null;

function resolveUpdaterAffordance(): UpdaterAffordance {
	if (Platform.isElectron && Updater.desktopUpdateInProgress) return 'updating';
	if (Platform.isElectron && Updater.desktopUpdateAvailable) return 'desktop';
	if (Platform.isElectron && Updater.nativeManualUpdateAvailable) return 'manual';
	if (Updater.updateInfo.web.available) return 'web';
	return null;
}

export function isUpdaterIconVisible(): boolean {
	return resolveUpdaterAffordance() != null;
}

export const UpdaterIcon = observer(() => {
	const {i18n} = useLingui();
	const handleClick = useCallback(() => {
		void Updater.applyUpdate();
	}, []);
	const affordance = resolveUpdaterAffordance();
	if (affordance == null) {
		return null;
	}
	const version = Updater.displayVersion;
	let tooltip: string;
	if (affordance === 'updating') {
		tooltip = i18n._(UPDATING_DESCRIPTOR);
	} else if (affordance === 'manual' && Updater.nativeManualDownloadOptions.length > 0) {
		tooltip = version ? i18n._(CHOOSE_LINUX_PACKAGE_DESCRIPTOR, {version}) : i18n._(CHOOSE_LINUX_PACKAGE_2_DESCRIPTOR);
	} else if (affordance !== 'web') {
		tooltip = i18n._(UPDATE_AVAILABLE_DESCRIPTOR);
	} else {
		tooltip = version
			? i18n._(CLICK_TO_RELOAD_AND_UPDATE_DESCRIPTOR, {version, productName: RuntimeConfig.productName})
			: i18n._(CLICK_TO_RELOAD_AND_UPDATE_2_DESCRIPTOR, {productName: RuntimeConfig.productName});
	}
	return (
		<Tooltip text={tooltip} position="bottom" data-flx="channel.channel-header-components.updater-icon.tooltip">
			<FocusRing offset={-2} data-flx="channel.channel-header-components.updater-icon.focus-ring">
				<button
					type="button"
					className={styles.updateIconButton}
					onClick={handleClick}
					disabled={affordance === 'updating'}
					aria-busy={affordance === 'updating'}
					aria-label={tooltip}
					data-flx="channel.channel-header-components.updater-icon.button.click"
				>
					<ArrowClockwiseIcon
						weight="bold"
						className={affordance === 'updating' ? styles.updateIconSpinning : styles.updateIcon}
						data-flx="channel.channel-header-components.updater-icon.update-icon"
					/>
				</button>
			</FocusRing>
		</Tooltip>
	);
});
