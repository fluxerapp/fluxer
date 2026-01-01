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

import {observer} from 'mobx-react-lite';
import styles from '~/components/channel/EmojiPicker.module.css';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';

interface StickerPickerInspectorProps {
	hoveredSticker: GuildStickerRecord | null;
}

export const StickerPickerInspector = observer(({hoveredSticker}: StickerPickerInspectorProps) => {
	return (
		<div className={styles.inspector}>
			{hoveredSticker && (
				<>
					<img src={hoveredSticker.url} alt={hoveredSticker.name} className={styles.inspectorEmoji} />
					<span className={styles.inspectorText}>{hoveredSticker.name}</span>
				</>
			)}
		</div>
	);
});
