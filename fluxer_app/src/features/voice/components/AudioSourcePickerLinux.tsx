// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {CheckboxItem, MenuGroupLabel} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	getLinuxAudioSourceDisplayName,
	type LinuxAudioSourceFilterOptions,
	type LinuxAudioSourceItem,
	linuxAudioSourceItemKey,
	mapLinuxAudioNodeToItems,
	uniqueLinuxAudioSourceItems,
} from '@app/features/voice/utils/LinuxAudioSourceRules';
import type {StreamSettingsShareContext} from '@app/features/voice/utils/StreamSettingsUpdatePolicy';
import type {VirtmicNode} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

const NO_AUDIO_DESCRIPTOR = msg({
	message: 'No audio',
	comment: 'Summary label in the Linux audio source picker when no audio sources are selected.',
});
const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Summary label in the Linux audio source picker when a custom subset of apps is selected.',
});
const APPS_DESCRIPTOR = msg({
	message: '{length} apps',
	comment:
		'Summary label in the Linux audio source picker when N apps are included. {length} is the integer app count.',
});
const ENTIRE_SYSTEM_DESCRIPTOR = msg({
	message: 'Entire system',
	comment: 'Summary label in the Linux audio source picker when capturing the whole system audio mix.',
});
const MICROPHONE_DESCRIPTOR = msg({
	message: 'Microphone',
	comment:
		'Summary label in the Linux audio source picker on a video device share, where the default is the microphone rather than the system mix.',
});
const MICROPHONE_WITH_DEVICE_DESCRIPTOR = msg({
	message: 'Microphone ({deviceLabel})',
	comment:
		'Capture option in the Linux audio source picker on a video device share. {deviceLabel} is the selected audio input device name.',
});
const AUDIO_SOURCES_DESCRIPTOR = msg({
	message: 'Audio sources: {summaryLabel}',
	comment:
		'Toggle / summary row label in the Linux audio source picker. {summaryLabel} is one of the audio-source summary labels.',
});
const CAPTURE_DESCRIPTOR = msg({
	message: 'Capture',
	comment:
		'Section header in the Linux audio source picker. Lists capture-target options (entire system / specific apps).',
});
const INCLUDE_APPS_DESCRIPTOR = msg({
	message: 'Include apps',
	comment: 'Section header in the Linux audio source picker for the per-app inclusion list.',
});
const EXCLUDE_FROM_SYSTEM_DESCRIPTOR = msg({
	message: 'Exclude from system',
	comment: 'Section header in the Linux audio source picker for apps excluded from the system mix.',
});
const logger = new Logger('AudioSourcePickerLinux');

interface AudioSourceSnapshot {
	available: boolean;
	loading: boolean;
	items: Array<LinuxAudioSourceItem>;
	hasPipewire: boolean;
	error: string | null;
}

const EMPTY_SNAPSHOT: AudioSourceSnapshot = {
	available: false,
	loading: true,
	items: [],
	hasPipewire: true,
	error: null,
};

function nodesEqual(a: VirtmicNode, b: VirtmicNode): boolean {
	const keysA = Object.keys(a);
	const keysB = Object.keys(b);
	if (keysA.length !== keysB.length) return false;
	return keysA.every((key) => a[key] === b[key]);
}

function isItemSelected(value: VirtmicNode, sources: Array<VirtmicNode>): boolean {
	return sources.some((source) => nodesEqual(source, value));
}

async function fetchAudioSources(options: LinuxAudioSourceFilterOptions): Promise<AudioSourceSnapshot> {
	const electronApi = getElectronAPI();
	if (!electronApi?.virtmic) {
		return {available: false, loading: false, items: [], hasPipewire: false, error: 'electron-unavailable'};
	}
	try {
		const availability = await electronApi.virtmic.getAvailability();
		if (!availability.available) {
			return {
				available: false,
				loading: false,
				items: [],
				hasPipewire: availability.reason !== 'no-pipewire',
				error: availability.reason ?? null,
			};
		}
		const result = await electronApi.virtmic.listTargets({granular: options.granular});
		if (!result.ok || !result.targets) {
			return {available: true, loading: false, items: [], hasPipewire: true, error: 'list-failed'};
		}
		const items = uniqueLinuxAudioSourceItems(
			result.targets.flatMap((node) => mapLinuxAudioNodeToItems(node, options)),
		);
		return {available: true, loading: false, items, hasPipewire: true, error: null};
	} catch (error) {
		logger.warn('Failed to fetch audio-bridge sources', error);
		return {available: false, loading: false, items: [], hasPipewire: true, error: 'exception'};
	}
}

interface AudioSourcePickerLinuxSubmenuProps {
	onSelectionChange?: () => void;
	shareContext?: StreamSettingsShareContext;
	microphoneLabel?: string;
}

export const AudioSourcePickerLinuxSubmenu = observer((props: AudioSourcePickerLinuxSubmenuProps) => {
	const {onSelectionChange, shareContext = 'display', microphoneLabel} = props;
	const {i18n} = useLingui();
	const isDeviceShare = shareContext === 'device';
	const sourceMode = VoiceSettings.getScreenShareAudioSourceMode();
	const includeSources = VoiceSettings.getScreenShareAudioIncludeSources();
	const excludeSources = VoiceSettings.getScreenShareAudioExcludeSources();
	const granular = VoiceSettings.getLinuxAudioCaptureGranularSelect();
	const deviceSelect = VoiceSettings.getLinuxAudioCaptureDeviceSelect();
	const ignoreVirtual = VoiceSettings.getLinuxAudioCaptureIgnoreVirtual();
	const [snapshot, setSnapshot] = useState<AudioSourceSnapshot>(EMPTY_SNAPSHOT);
	const refresh = useCallback(() => {
		setSnapshot((prev) => ({...prev, loading: true}));
		void fetchAudioSources({granular, deviceSelect, ignoreVirtual}).then((next) => {
			if (!next.available) {
				logger.warn('Hiding the audio source picker because no audio-bridge backend is available', {
					shareContext,
					reason: next.error,
					hasPipewire: next.hasPipewire,
				});
			}
			setSnapshot(next);
		});
	}, [granular, deviceSelect, ignoreVirtual, shareContext]);
	useEffect(() => {
		refresh();
	}, [refresh]);
	const handlePickSystem = useCallback(() => {
		VoiceSettingsCommands.update({
			screenShareAudioSourceMode: 'system',
			screenShareAudioIncludeSources: [],
		});
		onSelectionChange?.();
	}, [onSelectionChange]);
	const handlePickNone = useCallback(() => {
		VoiceSettingsCommands.update({
			screenShareAudioSourceMode: 'none',
			screenShareAudioIncludeSources: [],
		});
		onSelectionChange?.();
	}, [onSelectionChange]);
	const handleToggleApp = useCallback(
		(item: LinuxAudioSourceItem) => {
			const isSelected = isItemSelected(item.value, includeSources);
			const nextSources = isSelected
				? includeSources.filter((source) => !nodesEqual(source, item.value))
				: [...includeSources, item.value];
			VoiceSettingsCommands.update({
				screenShareAudioSourceMode: nextSources.length > 0 ? 'specific' : 'system',
				screenShareAudioIncludeSources: nextSources,
			});
			onSelectionChange?.();
		},
		[includeSources, onSelectionChange],
	);
	const handleToggleExcludeApp = useCallback(
		(item: LinuxAudioSourceItem) => {
			const isSelected = isItemSelected(item.value, excludeSources);
			const nextSources = isSelected
				? excludeSources.filter((source) => !nodesEqual(source, item.value))
				: [...excludeSources, item.value];
			VoiceSettingsCommands.update({
				screenShareAudioExcludeSources: nextSources,
			});
			onSelectionChange?.();
		},
		[excludeSources, onSelectionChange],
	);
	const defaultSourceLabel = isDeviceShare
		? i18n._(MICROPHONE_WITH_DEVICE_DESCRIPTOR, {deviceLabel: microphoneLabel ?? i18n._(MICROPHONE_DESCRIPTOR)})
		: i18n._(ENTIRE_SYSTEM_DESCRIPTOR);
	const summaryLabel =
		sourceMode === 'none'
			? i18n._(NO_AUDIO_DESCRIPTOR)
			: sourceMode === 'specific'
				? includeSources.length === 1
					? (getLinuxAudioSourceDisplayName(includeSources[0]) ?? i18n._(CUSTOM_DESCRIPTOR))
					: i18n._(APPS_DESCRIPTOR, {length: includeSources.length})
				: isDeviceShare
					? i18n._(MICROPHONE_DESCRIPTOR)
					: i18n._(ENTIRE_SYSTEM_DESCRIPTOR);
	if (!snapshot.available && !snapshot.loading) {
		return null;
	}
	return (
		<MenuItemSubmenu
			label={i18n._(AUDIO_SOURCES_DESCRIPTOR, {summaryLabel})}
			render={() => (
				<>
					<MenuGroup data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.menu-group">
						<MenuGroupLabel data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.group-label.capture">
							{i18n._(CAPTURE_DESCRIPTOR)}
						</MenuGroupLabel>
						<MenuItemRadio
							selected={sourceMode === 'system'}
							onSelect={handlePickSystem}
							data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.menu-item-radio.pick-system"
						>
							{isDeviceShare ? defaultSourceLabel : <Trans>Entire system audio</Trans>}
						</MenuItemRadio>
						<MenuItemRadio
							selected={sourceMode === 'none'}
							onSelect={handlePickNone}
							data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.menu-item-radio.pick-none"
						>
							<Trans>None</Trans>
						</MenuItemRadio>
					</MenuGroup>
					{snapshot.items.length > 0 && (
						<MenuGroup data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.menu-group--2">
							<MenuGroupLabel data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.group-label.include-apps">
								{i18n._(INCLUDE_APPS_DESCRIPTOR)}
							</MenuGroupLabel>
							{snapshot.items.map((item) => (
								<CheckboxItem
									key={linuxAudioSourceItemKey(item)}
									checked={isItemSelected(item.value, includeSources)}
									onCheckedChange={() => handleToggleApp(item)}
									data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.checkbox-item"
								>
									{item.name}
								</CheckboxItem>
							))}
						</MenuGroup>
					)}
					{!isDeviceShare && sourceMode === 'system' && snapshot.items.length > 0 && (
						<MenuGroup data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.menu-group--3">
							<MenuGroupLabel data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.group-label.exclude-from-system">
								{i18n._(EXCLUDE_FROM_SYSTEM_DESCRIPTOR)}
							</MenuGroupLabel>
							{snapshot.items.map((item) => (
								<CheckboxItem
									key={`exclude-${linuxAudioSourceItemKey(item)}`}
									checked={isItemSelected(item.value, excludeSources)}
									onCheckedChange={() => handleToggleExcludeApp(item)}
									data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.checkbox-item--2"
								>
									{item.name}
								</CheckboxItem>
							))}
						</MenuGroup>
					)}
					<MenuGroup data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.menu-group--4">
						<MenuItem
							onClick={refresh}
							closeOnSelect={false}
							data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.menu-item.refresh"
						>
							<Trans>Refresh audio sources</Trans>
						</MenuItem>
					</MenuGroup>
				</>
			)}
			data-flx="voice.audio-source-picker-linux.audio-source-picker-linux-submenu.menu-item-submenu"
		/>
	);
});

AudioSourcePickerLinuxSubmenu.displayName = 'AudioSourcePickerLinuxSubmenu';
