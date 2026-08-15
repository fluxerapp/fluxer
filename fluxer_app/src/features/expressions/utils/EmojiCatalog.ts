// SPDX-License-Identifier: AGPL-3.0-or-later

import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';

const EmojiCatalog = {
	normalizeShortcodeToSurrogate: (value: string): string => UnicodeEmojis.normalizeEmojiNameToSurrogate(value),
	getSurrogateName: (value: string): string | null => UnicodeEmojis.getSurrogateName(value),
};

export default EmojiCatalog;
