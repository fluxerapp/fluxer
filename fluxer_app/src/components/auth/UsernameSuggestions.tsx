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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import styles from './UsernameSuggestions.module.css';

interface UsernameSuggestionsProps {
	suggestions: Array<string>;
	onSelect: (username: string) => void;
}

export const UsernameSuggestions = observer(function UsernameSuggestions({
	suggestions,
	onSelect,
}: UsernameSuggestionsProps) {
	if (suggestions.length === 0) {
		return null;
	}

	return (
		<div className={styles.container}>
			<p className={styles.label}>
				<Trans>Suggested usernames:</Trans>
			</p>
			<div className={styles.suggestionsList}>
				{suggestions.map((suggestion, index) => (
					<button
						key={suggestion}
						type="button"
						onClick={() => onSelect(suggestion)}
						className={styles.suggestionButton}
						style={{
							animationDelay: `${index * 50}ms`,
						}}
					>
						{suggestion}
					</button>
				))}
			</div>
		</div>
	);
});
