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

import {useEffect, useState} from 'react';
import styles from './IncomingCallManager.module.css';

let portalRoot: HTMLElement | null = null;

function createPortalRoot(): HTMLElement | null {
	if (portalRoot && document.body.contains(portalRoot)) {
		return portalRoot;
	}

	const root = document.createElement('div');
	root.className = styles.portalRoot;
	root.dataset.incomingCallPortal = 'true';
	root.dataset.floatingUiPortal = 'true';
	document.body.appendChild(root);
	portalRoot = root;
	return root;
}

export function ensureIncomingCallPortalRoot(): HTMLElement | null {
	return createPortalRoot();
}

export function useIncomingCallPortalRoot(): HTMLElement | null {
	const [root, setRoot] = useState<HTMLElement | null>(() => createPortalRoot());

	useEffect(() => {
		setRoot((prev) => prev ?? createPortalRoot());
	}, []);

	return root;
}
