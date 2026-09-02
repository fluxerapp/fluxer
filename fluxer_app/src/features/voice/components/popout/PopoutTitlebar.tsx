// SPDX-License-Identifier: AGPL-3.0-or-later

import nativeTitlebarStyles from '@app/features/app/components/layout/NativeTitlebar.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getNativePlatformSync} from '@app/features/ui/utils/NativeUtils';
import styles from '@app/features/voice/components/popout/PopoutTitlebar.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowSquareInIcon, PushPinIcon, PushPinSlashIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useMemo} from 'react';

const STAY_ON_TOP_DESCRIPTOR = msg({
	message: 'Stay on top',
	comment: 'Button label in a popped-out voice window titlebar that pins the window above other windows.',
});
const REMOVE_FROM_TOP_DESCRIPTOR = msg({
	message: 'Remove from top',
	comment: 'Button label in a popped-out voice window titlebar that unpins the window from staying above others.',
});
const POP_BACK_IN_DESCRIPTOR = msg({
	message: 'Pop back in',
	comment: 'Button label in a popped-out voice window titlebar that restores the content into the main app window.',
});
const CLOSE_WINDOW_DESCRIPTOR = msg({
	message: 'Close window',
	comment: 'Button label in a popped-out voice window titlebar that closes the popped-out window.',
});

const NATIVE_SYSTEM_TITLEBAR_CLASS = 'native-system-titlebar';

interface PopoutTitlebarProps {
	title: string;
	showTitle?: boolean;
	isAlwaysOnTop: boolean;
	onToggleAlwaysOnTop?: () => void;
	onRestore: () => void;
	onClose: () => void;
}

export const PopoutTitlebar: React.FC<PopoutTitlebarProps> = ({
	title,
	showTitle = true,
	isAlwaysOnTop,
	onToggleAlwaysOnTop,
	onRestore,
	onClose,
}) => {
	const {i18n} = useLingui();
	const platform = useMemo(() => getNativePlatformSync(), []);
	const usesSystemChrome =
		platform === 'macos' || document.documentElement.classList.contains(NATIVE_SYSTEM_TITLEBAR_CLASS);
	const pinLabel = isAlwaysOnTop ? i18n._(REMOVE_FROM_TOP_DESCRIPTOR) : i18n._(STAY_ON_TOP_DESCRIPTOR);
	const PinIcon = isAlwaysOnTop ? PushPinSlashIcon : PushPinIcon;
	return (
		<div
			role="group"
			className={clsx(styles.titlebar, platform === 'macos' && styles.titlebarMac)}
			data-platform={platform}
			data-flx="voice.popout-titlebar.titlebar"
		>
			{showTitle && (
				<span className={styles.title} data-flx="voice.popout-titlebar.title">
					{title}
				</span>
			)}
			<div role="group" className={styles.actions} data-flx="voice.popout-titlebar.actions">
				{onToggleAlwaysOnTop && (
					<FocusRing offset={-2} data-flx="voice.popout-titlebar.focus-ring.pin">
						<button
							type="button"
							className={clsx(nativeTitlebarStyles.controlButton, isAlwaysOnTop && styles.actionButtonActive)}
							onClick={onToggleAlwaysOnTop}
							aria-pressed={isAlwaysOnTop}
							aria-label={pinLabel}
							title={pinLabel}
							data-flx="voice.popout-titlebar.control-button.toggle-always-on-top"
						>
							<PinIcon weight="bold" data-flx="voice.popout-titlebar.pin-icon" />
						</button>
					</FocusRing>
				)}
				<FocusRing offset={-2} data-flx="voice.popout-titlebar.focus-ring.restore">
					<button
						type="button"
						className={nativeTitlebarStyles.controlButton}
						onClick={onRestore}
						aria-label={i18n._(POP_BACK_IN_DESCRIPTOR)}
						title={i18n._(POP_BACK_IN_DESCRIPTOR)}
						data-flx="voice.popout-titlebar.control-button.restore"
					>
						<ArrowSquareInIcon weight="bold" data-flx="voice.popout-titlebar.arrow-square-in-icon" />
					</button>
				</FocusRing>
				{!usesSystemChrome && (
					<FocusRing offset={-2} data-flx="voice.popout-titlebar.focus-ring.close">
						<button
							type="button"
							className={clsx(nativeTitlebarStyles.controlButton, nativeTitlebarStyles.closeButton)}
							onClick={onClose}
							aria-label={i18n._(CLOSE_WINDOW_DESCRIPTOR)}
							data-flx="voice.popout-titlebar.control-button.close"
						>
							<XIcon weight="bold" data-flx="voice.popout-titlebar.x-icon" />
						</button>
					</FocusRing>
				)}
			</div>
		</div>
	);
};
