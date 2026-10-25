// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import styles from '@app/features/music/components/MusicPanel.module.css';
import MusicStore from '@app/features/music/state/MusicStore';
import {useStoreVersion} from '@app/features/voice/engine/Store';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MusicNotesIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const MUSIC_DESCRIPTOR = msg({
	message: 'Music',
	comment: 'Tooltip / aria label on the voice call button that opens the music panel.',
});

interface MusicButtonProps {
	className?: string;
}

export const MusicButton = observer(function MusicButton({className}: MusicButtonProps) {
	const {i18n} = useLingui();
	useStoreVersion(MusicStore);
	const playing = MusicStore.state?.status === 'playing';
	return (
		<span className={styles.buttonWrap} data-flx="music.music-button.wrap">
			<ChannelHeaderIcon
				icon={MusicNotesIcon}
				label={i18n._(MUSIC_DESCRIPTOR)}
				className={className}
				isSelected={MusicStore.panelOpen}
				onClick={() => MusicStore.togglePanel()}
				data-flx="music.music-button.channel-header-icon"
			/>
			{playing && <span className={styles.playingDot} data-flx="music.music-button.playing-dot" />}
		</span>
	);
});
