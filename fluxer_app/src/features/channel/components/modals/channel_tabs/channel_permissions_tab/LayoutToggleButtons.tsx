// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/modals/channel_tabs/ChannelPermissionsTab.module.css';
import PermissionLayout from '@app/features/permissions/state/PermissionLayout';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GridFourIcon, ListIcon, RowsIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DENSE_LAYOUT_DESCRIPTOR = msg({
	message: 'Dense layout',
	comment: 'Density mode label in the channel permissions tab layout toggle. Dense option.',
});
const COMFY_LAYOUT_DESCRIPTOR = msg({
	message: 'Comfy layout',
	comment: 'Density mode label in the channel permissions tab layout toggle. Comfortable option.',
});
const SWITCH_TO_DENSE_LAYOUT_DESCRIPTOR = msg({
	message: 'Switch to dense layout',
	comment: 'Tooltip on the density toggle when currently in comfy mode.',
});
const SWITCH_TO_COMFY_LAYOUT_DESCRIPTOR = msg({
	message: 'Switch to comfy layout',
	comment: 'Tooltip on the density toggle when currently in dense mode.',
});
const SINGLE_COLUMN_DESCRIPTOR = msg({
	message: 'Single column',
	comment: 'Column count label in the channel permissions tab layout toggle.',
});
const TWO_COLUMNS_DESCRIPTOR = msg({
	message: 'Two columns',
	comment: 'Column count label in the channel permissions tab layout toggle.',
});
const SWITCH_TO_SINGLE_COLUMN_DESCRIPTOR = msg({
	message: 'Switch to single column',
	comment: 'Tooltip on the column count toggle when currently in two-column mode.',
});
const SWITCH_TO_TWO_COLUMNS_DESCRIPTOR = msg({
	message: 'Switch to two columns',
	comment: 'Tooltip on the column count toggle when currently in single-column mode.',
});
export const LayoutToggleButtons: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<div className={styles.layoutButtons} data-flx="channel.channel-tabs.channel-permissions-tab.layout-buttons">
			<Tooltip
				text={PermissionLayout.isComfy ? i18n._(DENSE_LAYOUT_DESCRIPTOR) : i18n._(COMFY_LAYOUT_DESCRIPTOR)}
				data-flx="channel.channel-tabs.channel-permissions-tab.tooltip"
			>
				<button
					type="button"
					className={styles.layoutButton}
					onClick={() => PermissionLayout.toggleLayoutMode()}
					aria-label={
						PermissionLayout.isComfy
							? i18n._(SWITCH_TO_DENSE_LAYOUT_DESCRIPTOR)
							: i18n._(SWITCH_TO_COMFY_LAYOUT_DESCRIPTOR)
					}
					data-flx="channel.channel-tabs.channel-permissions-tab.layout-button.toggle-layout-mode"
				>
					{PermissionLayout.isComfy ? (
						<RowsIcon
							size={remFromPx(20)}
							weight="bold"
							data-flx="channel.channel-tabs.channel-permissions-tab.rows-icon"
						/>
					) : (
						<ListIcon
							size={remFromPx(20)}
							weight="bold"
							data-flx="channel.channel-tabs.channel-permissions-tab.list-icon"
						/>
					)}
				</button>
			</Tooltip>
			<Tooltip
				text={PermissionLayout.isGrid ? i18n._(SINGLE_COLUMN_DESCRIPTOR) : i18n._(TWO_COLUMNS_DESCRIPTOR)}
				data-flx="channel.channel-tabs.channel-permissions-tab.tooltip--2"
			>
				<button
					type="button"
					className={styles.layoutButton}
					onClick={() => PermissionLayout.toggleGridMode()}
					aria-label={
						PermissionLayout.isGrid
							? i18n._(SWITCH_TO_SINGLE_COLUMN_DESCRIPTOR)
							: i18n._(SWITCH_TO_TWO_COLUMNS_DESCRIPTOR)
					}
					data-flx="channel.channel-tabs.channel-permissions-tab.layout-button.toggle-grid-mode"
				>
					<GridFourIcon
						size={remFromPx(20)}
						weight={PermissionLayout.isGrid ? 'fill' : 'bold'}
						data-flx="channel.channel-tabs.channel-permissions-tab.grid-four-icon"
					/>
				</button>
			</Tooltip>
		</div>
	);
});
