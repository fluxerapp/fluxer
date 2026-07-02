// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelHeader.module.css';
import {ThreadsPopout} from '@app/features/channel/components/popouts/ThreadsPopout';
import type {Channel} from '@app/features/channel/models/Channel';
import {ThreadIcon} from '@app/features/ui/components/icons/ThreadIcon';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {clsx} from 'clsx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const THREADS_DESCRIPTOR = msg({
	message: 'Threads',
	comment: 'Accessible label on the threads button in the channel header.',
});

interface ThreadsButtonProps {
	channel: Channel;
	'data-flx'?: string;
}

export const ThreadsButton = observer(({channel}: ThreadsButtonProps) => {
	const {i18n} = useLingui();
	const {isOpen, openProps} = usePopout('channel-threads');
	const guildId = channel.guildId ?? '';
	const label = i18n._(THREADS_DESCRIPTOR);

	return (
		<Popout
			{...openProps}
			render={({onClose}) => (
				<ThreadsPopout
					channelId={channel.id}
					guildId={guildId}
					onClose={onClose}
					data-flx="channel.channel-header-components.threads-button.threads-popout"
				/>
			)}
			position="bottom-end"
			tooltip={label}
			tooltipPosition="bottom"
			data-flx="channel.channel-header-components.threads-button.popout"
		>
			<div
				className={styles.iconButtonWrapper}
				data-flx="channel.channel-header-components.threads-button.wrapper"
			>
				<FocusRing offset={-2} data-flx="channel.channel-header-components.threads-button.focus-ring">
					<button
						type="button"
						className={clsx(isOpen ? styles.iconButtonSelected : styles.iconButtonDefault)}
						aria-label={label}
						aria-pressed={isOpen}
						aria-haspopup={true}
						aria-expanded={isOpen}
						data-flx="channel.channel-header-components.threads-button.button"
					>
						<ThreadIcon size={20} className={styles.buttonIcon} data-flx="channel.channel-header-components.threads-button.icon" />
					</button>
				</FocusRing>
			</div>
		</Popout>
	);
});
