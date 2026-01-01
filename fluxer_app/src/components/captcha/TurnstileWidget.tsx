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

import {Turnstile} from '@marsidev/react-turnstile';
import {observer} from 'mobx-react-lite';

interface TurnstileWidgetProps {
	sitekey: string;
	onVerify: (token: string) => void;
	onError?: (error: string) => void;
	onExpire?: () => void;
	theme?: 'light' | 'dark' | 'auto';
}

export const TurnstileWidget = observer(
	({sitekey, onVerify, onError, onExpire, theme = 'dark'}: TurnstileWidgetProps) => {
		return (
			<Turnstile
				siteKey={sitekey}
				onSuccess={onVerify}
				onError={() => onError?.('Turnstile error')}
				onExpire={onExpire}
				options={{
					theme,
				}}
			/>
		);
	},
);
