// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/NativeTitlebar.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CopySimpleIcon, MinusIcon, SquareIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import {useEffect, useState} from 'react';

const MINIMIZE_WINDOW_DESCRIPTOR = msg({
	message: 'Minimize window',
	comment: 'Short label in the app layout native titlebar.',
});
const RESTORE_WINDOW_DESCRIPTOR = msg({
	message: 'Restore window',
	comment: 'Short label in the app layout native titlebar.',
});
const MAXIMIZE_WINDOW_DESCRIPTOR = msg({
	message: 'Maximize window',
	comment: 'Short label in the app layout native titlebar.',
});
const CLOSE_WINDOW_DESCRIPTOR = msg({
	message: 'Close window',
	comment: 'Short label in the app layout native titlebar.',
});

interface NativeWindowControlsProps {
	className?: string;
	'data-flx'?: string;
}

export const NativeWindowControls: React.FC<NativeWindowControlsProps> = ({
	className,
	'data-flx': dataFlx = 'app.native-titlebar.controls',
}) => {
	const {i18n} = useLingui();
	const [isMaximized, setIsMaximized] = useState(false);
	useEffect(() => {
		const electronApi = getElectronAPI();
		if (!electronApi) return;
		let disposed = false;
		const updateMaximized = (maximized: boolean) => {
			if (disposed) return;
			setIsMaximized(maximized);
		};
		const unsubscribe = electronApi.onWindowMaximizeChange?.(updateMaximized);
		void electronApi
			.windowIsMaximized?.()
			.then(updateMaximized)
			.catch(() => undefined);
		return () => {
			disposed = true;
			unsubscribe?.();
		};
	}, []);
	const handleMinimize = () => {
		const electronApi = getElectronAPI();
		electronApi?.windowMinimize?.();
	};
	const handleToggleMaximize = () => {
		const electronApi = getElectronAPI();
		if (!electronApi?.windowMaximize) return;
		electronApi.windowMaximize();
	};
	const handleClose = () => {
		const electronApi = getElectronAPI();
		electronApi?.windowClose?.();
	};
	const handleControlsDoubleClick = (event: React.MouseEvent<HTMLDivElement>) => {
		event.stopPropagation();
	};
	return (
		<div
			role="group"
			className={clsx(styles.controls, className)}
			onDoubleClick={handleControlsDoubleClick}
			data-no-drag=""
			data-flx={dataFlx}
		>
			<FocusRing offset={-2} data-flx="app.native-titlebar.focus-ring">
				<button
					type="button"
					tabIndex={-1}
					className={styles.controlButton}
					onClick={handleMinimize}
					aria-label={i18n._(MINIMIZE_WINDOW_DESCRIPTOR)}
					data-flx="app.native-titlebar.control-button.minimize"
				>
					<MinusIcon weight="bold" data-flx="app.native-titlebar.minus-icon" />
				</button>
			</FocusRing>
			<FocusRing offset={-2} data-flx="app.native-titlebar.focus-ring--2">
				<button
					type="button"
					tabIndex={-1}
					className={styles.controlButton}
					onClick={handleToggleMaximize}
					aria-label={isMaximized ? i18n._(RESTORE_WINDOW_DESCRIPTOR) : i18n._(MAXIMIZE_WINDOW_DESCRIPTOR)}
					data-flx="app.native-titlebar.control-button.toggle-maximize"
				>
					{isMaximized ? (
						<CopySimpleIcon weight="bold" data-flx="app.native-titlebar.copy-simple-icon" />
					) : (
						<SquareIcon weight="bold" data-flx="app.native-titlebar.square-icon" />
					)}
				</button>
			</FocusRing>
			<FocusRing offset={-2} data-flx="app.native-titlebar.focus-ring--3">
				<button
					type="button"
					tabIndex={-1}
					className={clsx(styles.controlButton, styles.closeButton)}
					onClick={handleClose}
					aria-label={i18n._(CLOSE_WINDOW_DESCRIPTOR)}
					data-flx="app.native-titlebar.control-button.close"
				>
					<XIcon weight="bold" data-flx="app.native-titlebar.x-icon" />
				</button>
			</FocusRing>
		</div>
	);
};
