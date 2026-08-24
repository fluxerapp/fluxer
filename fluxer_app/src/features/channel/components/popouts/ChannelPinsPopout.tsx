// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelPinsContent} from '@app/features/app/components/shared/ChannelPinsContent';
import styles from '@app/features/channel/components/popouts/ChannelPinsPopout.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {RESIZABLE_PANE_DEFAULT_VIEWPORT_PADDING, useResizablePane} from '@app/features/ui/hooks/useResizablePane';
import {
	type ResizablePaneHandleLabels,
	ResizablePaneHandles,
} from '@app/features/ui/resizable_pane/ResizablePaneHandles';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PushPinIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useRef} from 'react';

const PINNED_MESSAGES_DESCRIPTOR = msg({
	message: 'Pinned messages',
	comment: 'Button or menu action label in the channel pins popout. Keep it concise.',
});
const RESIZE_PINS_TOP_DESCRIPTOR = msg({
	message: 'Resize pinned messages from the top edge',
	comment: 'Accessible label for the top resize handle on the channel pins popout.',
});
const RESIZE_PINS_BOTTOM_DESCRIPTOR = msg({
	message: 'Resize pinned messages from the bottom edge',
	comment: 'Accessible label for the bottom resize handle on the channel pins popout.',
});
const RESIZE_PINS_LEFT_DESCRIPTOR = msg({
	message: 'Resize pinned messages from the left edge',
	comment: 'Accessible label for the left resize handle on the channel pins popout.',
});
const RESIZE_PINS_RIGHT_DESCRIPTOR = msg({
	message: 'Resize pinned messages from the right edge',
	comment: 'Accessible label for the right resize handle on the channel pins popout.',
});
const RESIZE_PINS_TOP_LEFT_DESCRIPTOR = msg({
	message: 'Resize pinned messages from top left',
	comment: 'Accessible label for the top-left resize handle on the channel pins popout.',
});
const RESIZE_PINS_TOP_RIGHT_DESCRIPTOR = msg({
	message: 'Resize pinned messages from top right',
	comment: 'Accessible label for the top-right resize handle on the channel pins popout.',
});
const RESIZE_PINS_BOTTOM_LEFT_DESCRIPTOR = msg({
	message: 'Resize pinned messages from bottom left',
	comment: 'Accessible label for the bottom-left resize handle on the channel pins popout.',
});
const RESIZE_PINS_BOTTOM_RIGHT_DESCRIPTOR = msg({
	message: 'Resize pinned messages from bottom right',
	comment: 'Accessible label for the bottom-right resize handle on the channel pins popout.',
});

const CHANNEL_PINS_POPOUT_DEFAULT_SIZE = {width: 480, height: 640};
const CHANNEL_PINS_POPOUT_MIN_SIZE = {width: 320, height: 200};
const CHANNEL_PINS_POPOUT_RESIZING_CLASS = 'channel-pins-popout-resizing';
const CHANNEL_PINS_POPOUT_RESIZE_CURSOR_PROPERTY = '--channel-pins-popout-resize-cursor';
const CHANNEL_PINS_RESIZE_HANDLE_LABELS: ResizablePaneHandleLabels = {
	top: RESIZE_PINS_TOP_DESCRIPTOR,
	bottom: RESIZE_PINS_BOTTOM_DESCRIPTOR,
	left: RESIZE_PINS_LEFT_DESCRIPTOR,
	right: RESIZE_PINS_RIGHT_DESCRIPTOR,
	'top-left': RESIZE_PINS_TOP_LEFT_DESCRIPTOR,
	'top-right': RESIZE_PINS_TOP_RIGHT_DESCRIPTOR,
	'bottom-left': RESIZE_PINS_BOTTOM_LEFT_DESCRIPTOR,
	'bottom-right': RESIZE_PINS_BOTTOM_RIGHT_DESCRIPTOR,
};

export const ChannelPinsPopout = observer(({channel, onClose}: {channel: Channel; onClose?: () => void}) => {
	const {i18n} = useLingui();
	const containerRef = useRef<HTMLDivElement | null>(null);
	const {size, getHandleProps} = useResizablePane(containerRef, {
		storageKey: 'fluxer:ui:channel-pins-popout-size',
		defaultSize: CHANNEL_PINS_POPOUT_DEFAULT_SIZE,
		minSize: CHANNEL_PINS_POPOUT_MIN_SIZE,
		viewportPadding: RESIZABLE_PANE_DEFAULT_VIEWPORT_PADDING,
		resizingClassName: CHANNEL_PINS_POPOUT_RESIZING_CLASS,
		cursorProperty: CHANNEL_PINS_POPOUT_RESIZE_CURSOR_PROPERTY,
	});
	return (
		<div
			ref={containerRef}
			className={styles.container}
			style={{width: remFromPx(size.width), height: remFromPx(size.height)}}
			data-flx="channel.channel-pins-popout.container"
		>
			<div className={styles.header} data-flx="channel.channel-pins-popout.header">
				<PushPinIcon className={styles.iconLarge} data-flx="channel.channel-pins-popout.icon-large" />
				<h1 className={styles.title} data-flx="channel.channel-pins-popout.title">
					{i18n._(PINNED_MESSAGES_DESCRIPTOR)}
				</h1>
			</div>
			<div className={styles.body} data-flx="channel.channel-pins-popout.body">
				<ChannelPinsContent
					channel={channel}
					onJump={onClose}
					data-flx="channel.channel-pins-popout.channel-pins-content"
				/>
			</div>
			<ResizablePaneHandles
				getHandleProps={getHandleProps}
				labels={CHANNEL_PINS_RESIZE_HANDLE_LABELS}
				data-flx="channel.channel-pins-popout.resizable-pane-handles"
			/>
		</div>
	);
});
