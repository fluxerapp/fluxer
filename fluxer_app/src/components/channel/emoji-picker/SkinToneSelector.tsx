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

import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as EmojiActionCreators from '~/actions/EmojiActionCreators';
import {SKIN_TONE_SURROGATES} from '~/Constants';
import styles from '~/components/channel/EmojiPicker.module.css';
import {EMOJI_CLAP} from '~/components/channel/emoji-picker/EmojiPickerConstants';
import {ComponentDispatch} from '~/lib/ComponentDispatch';
import EmojiStore from '~/stores/EmojiStore';
import * as EmojiUtils from '~/utils/EmojiUtils';
import {shouldUseNativeEmoji} from '~/utils/EmojiUtils';

interface SkinTonePickerProps {
	isOpen: boolean;
	onClose: () => void;
	skinTone: string;
}

const SkinTonePicker = observer(({isOpen, onClose, skinTone}: SkinTonePickerProps) => {
	const handleSelect = (surrogate: string) => {
		EmojiActionCreators.setSkinTone(surrogate);
		ComponentDispatch.dispatch('EMOJI_PICKER_RERENDER');
		onClose();
	};

	return (
		<AnimatePresence>
			{isOpen && (
				<motion.div
					initial={{opacity: 0, height: 0}}
					animate={{opacity: 1, height: 'auto'}}
					exit={{opacity: 0, height: 0}}
					className={styles.skinTonePickerOptions}
				>
					{[skinTone, ...['', ...SKIN_TONE_SURROGATES].filter((surrogate) => surrogate !== skinTone)].map(
						(surrogate, index) => {
							const emojiChar = EMOJI_CLAP + surrogate;
							const emojiUrl = EmojiUtils.getEmojiURL(emojiChar);
							return (
								<motion.button
									key={surrogate || 'default'}
									type="button"
									initial={{opacity: 0, scale: index === 0 ? 1 : 0}}
									animate={{opacity: 1, scale: 1}}
									exit={{opacity: 0, scale: 0}}
									className={styles.skinTonePickerItem}
									onClick={() => handleSelect(surrogate)}
								>
									{shouldUseNativeEmoji ? (
										<span className={styles.skinToneNativeEmoji}>{emojiChar}</span>
									) : (
										<div
											className={styles.skinTonePickerItemImage}
											style={{backgroundImage: emojiUrl ? `url(${emojiUrl})` : undefined}}
										/>
									)}
								</motion.button>
							);
						},
					)}
				</motion.div>
			)}
		</AnimatePresence>
	);
});

interface SkinTonePickerButtonProps {
	onClick: () => void;
	selectedEmojiURL: string | null;
	selectedEmojiChar: string;
}

const SkinTonePickerButton = observer(({onClick, selectedEmojiURL, selectedEmojiChar}: SkinTonePickerButtonProps) => (
	<motion.button
		type="button"
		className={styles.skinTonePickerButton}
		onClick={onClick}
		style={!shouldUseNativeEmoji && selectedEmojiURL ? {backgroundImage: `url(${selectedEmojiURL})`} : undefined}
		initial={{opacity: 1, scale: 1}}
		animate={{opacity: 1, scale: 1}}
		exit={{opacity: 0, scale: 0}}
	>
		{shouldUseNativeEmoji && <span className={styles.skinToneNativeEmoji}>{selectedEmojiChar}</span>}
	</motion.button>
));

export const SkinToneSelector = observer(() => {
	const [isOpen, setIsOpen] = React.useState(false);
	const skinTone = EmojiStore.skinTone;
	const selectedEmojiChar = EMOJI_CLAP + skinTone;
	const selectedEmojiUrl = EmojiUtils.getEmojiURL(selectedEmojiChar);
	const selectorRef = React.useRef<HTMLDivElement | null>(null);

	const handleClickOutside = React.useCallback((event: MouseEvent) => {
		if (selectorRef.current && !selectorRef.current.contains(event.target as Node)) {
			setIsOpen(false);
		}
	}, []);

	React.useEffect(() => {
		document.addEventListener('mousedown', handleClickOutside);
		return () => {
			document.removeEventListener('mousedown', handleClickOutside);
		};
	}, [handleClickOutside]);

	return (
		<div ref={selectorRef} className={styles.skinToneSelectorContainer}>
			<SkinTonePicker isOpen={isOpen} onClose={() => setIsOpen(false)} skinTone={skinTone} />
			<SkinTonePickerButton
				onClick={() => setIsOpen(true)}
				selectedEmojiURL={selectedEmojiUrl}
				selectedEmojiChar={selectedEmojiChar}
			/>
		</div>
	);
});
