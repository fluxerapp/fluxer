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
import {DownloadSimpleIcon, InfoIcon} from '@phosphor-icons/react';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {Switch} from '~/components/form/Switch';
import {KeybindRecorder} from '~/components/keybinds/KeybindRecorder';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Button} from '~/components/uikit/Button/Button';
import {IS_DEV} from '~/lib/env';
import KeybindManager from '~/lib/KeybindManager';
import KeybindStore, {getDefaultKeybind, type KeybindConfig, type KeyCombo} from '~/stores/KeybindStore';
import NativePermissionStore from '~/stores/NativePermissionStore';
import {InputMonitoringSection} from './components/InputMonitoringSection';
import styles from './KeybindsTab.module.css';

const KeybindRow = observer(
	({entry, isNativeDesktop}: {entry: KeybindConfig & {combo: KeyCombo}; isNativeDesktop: boolean}) => {
		const {t, i18n} = useLingui();
		const handleToggleGlobal = (value: boolean) => {
			KeybindStore.toggleGlobal(entry.action, value);
		};
		const defaultCombo = getDefaultKeybind(entry.action, i18n);
		const downloadUrl = 'https://fluxer.app/download';

		return (
			<div className={`${styles.rowWrapper} ${entry.allowGlobal ? styles.hasFooterWrapper : ''}`}>
				<div className={styles.row}>
					<div className={styles.rowText}>
						<div className={styles.label}>{entry.label}</div>
						{entry.description ? <div className={styles.rowDescription}>{entry.description}</div> : null}
					</div>
					<div className={styles.rowControls}>
						<div className={styles.primaryControls}>
							<KeybindRecorder
								action={entry.action}
								value={entry.combo}
								defaultValue={defaultCombo}
								onChange={(combo) => {
									KeybindStore.setKeybind(entry.action, {...combo, global: entry.combo.global});
								}}
								onClear={() => {
									KeybindStore.setKeybind(entry.action, {
										key: '',
										code: '',
										global: entry.combo.global,
										enabled: false,
									});
								}}
								onReset={() => {
									if (defaultCombo) {
										KeybindStore.setKeybind(entry.action, {...defaultCombo, global: entry.combo.global});
									}
								}}
							/>
						</div>
					</div>
				</div>

				{entry.allowGlobal ? (
					<div
						className={clsx(
							styles.globalFooter,
							isNativeDesktop ? styles.globalFooterDesktop : styles.globalFooterBrand,
						)}
					>
						<div className={styles.globalFooterText}>
							<span className={styles.globalLabel}>
								<Trans>Global shortcut</Trans>
							</span>
							<span className={styles.globalDescription}>
								<Trans>Run this shortcut even when Fluxer is not focused.</Trans>
							</span>
						</div>
						{isNativeDesktop ? (
							<div className={styles.globalFooterControls}>
								<Switch
									value={entry.combo.global ?? false}
									onChange={handleToggleGlobal}
									ariaLabel={t`Use this shortcut system-wide`}
								/>
							</div>
						) : (
							<div className={styles.globalFooterControls}>
								<Button
									variant="inverted"
									small
									leftIcon={<DownloadSimpleIcon size={16} weight="fill" />}
									onClick={() => window.open(downloadUrl, '_blank', 'noopener')}
									title={t`Global shortcuts are available in the desktop app`}
								>
									<Trans>Get desktop app</Trans>
								</Button>
							</div>
						)}
					</div>
				) : null}
			</div>
		);
	},
);

const KeybindsTab: React.FC = observer(() => {
	const {t} = useLingui();
	const keybinds = KeybindStore.getAll();
	const categories: Array<{id: KeybindConfig['category']; title: string}> = [
		{id: 'navigation', title: t`Navigation`},
		{id: 'voice', title: t`Voice`},
		{id: 'messaging', title: t`Messaging`},
		{id: 'popouts', title: t`Popouts`},
		{id: 'calls', title: t`Calls`},
		{id: 'system', title: t`System`},
	];

	const [devDesktopOverride, setDevDesktopOverride] = React.useState(false);
	const isNativeDesktop = IS_DEV
		? NativePermissionStore.isDesktop || devDesktopOverride
		: NativePermissionStore.isDesktop;

	React.useEffect(() => {
		KeybindManager.suspend();
		return () => {
			KeybindManager.resume();
		};
	}, []);

	const handleResetToDefaults = () => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Reset Keyboard Shortcuts`}
					description={t`Are you sure you want to reset all keyboard shortcuts to their default values?`}
					primaryText={t`Reset`}
					primaryVariant="danger-primary"
					onPrimary={() => {
						KeybindStore.resetToDefaults();
					}}
				/>
			)),
		);
	};

	const filteredKeybinds = keybinds.filter((k) => k.action !== 'push_to_talk');

	return (
		<div className={styles.container}>
			<div className={styles.callout}>
				<div className={styles.calloutTitle}>
					<InfoIcon size={18} weight="fill" className={styles.calloutIcon} />
					<Trans>Shortcuts are paused while editing</Trans>
				</div>
			</div>

			<div className={styles.headerRow}>
				<div className={styles.header}>
					<h2 className={styles.title}>
						<Trans>Keyboard Shortcuts</Trans>
					</h2>
					<p className={styles.description}>
						<Trans>Customize keyboard shortcuts for navigation and actions.</Trans>
					</p>
				</div>
				<div className={styles.headerActions}>
					<Button variant="secondary" small={true} type="button" onClick={handleResetToDefaults}>
						<Trans>Reset to defaults</Trans>
					</Button>
				</div>
			</div>

			{IS_DEV ? (
				<div className={styles.devToggleRow}>
					<div className={styles.devToggle}>
						<Switch value={devDesktopOverride} onChange={setDevDesktopOverride} ariaLabel={t`Mock desktop mode`} />
						<span className={styles.devToggleLabel}>
							<Trans>Mock desktop</Trans>
						</span>
					</div>
				</div>
			) : null}

			<InputMonitoringSection />

			{categories
				.map((category) => ({
					...category,
					entries: filteredKeybinds.filter((k) => k.category === category.id),
				}))
				.filter((c) => c.entries.length > 0)
				.map((category) => (
					<div className={styles.section} key={category.id}>
						<div className={styles.sectionTitle}>{category.title}</div>
						{category.entries.map((entry) => (
							<KeybindRow key={entry.action} entry={entry} isNativeDesktop={isNativeDesktop} />
						))}
					</div>
				))}
		</div>
	);
});

export default KeybindsTab;
