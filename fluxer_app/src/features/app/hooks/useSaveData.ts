// SPDX-License-Identifier: AGPL-3.0-or-later

import {useEffect, useState} from 'react';

type ConnectionLike = {
	saveData?: boolean;
	addEventListener?: (event: string, cb: () => void) => void;
	removeEventListener?: (event: string, cb: () => void) => void;
};

const getConnection = (): ConnectionLike | null => {
	if (typeof navigator === 'undefined') return null;
	const nav = navigator as Navigator & {connection?: ConnectionLike};
	return nav.connection ?? null;
};

const readSaveData = (): boolean => getConnection()?.saveData === true;

const saveDataListeners = new Set<(value: boolean) => void>();

let cachedSaveData = readSaveData();
let detachConnectionListener: (() => void) | null = null;

function publishSaveData(): void {
	const next = readSaveData();
	if (next === cachedSaveData) return;
	cachedSaveData = next;
	for (const listener of saveDataListeners) listener(next);
}

export function subscribeSaveData(listener: (value: boolean) => void): () => void {
	saveDataListeners.add(listener);
	if (detachConnectionListener === null) {
		const connection = getConnection();
		if (connection?.addEventListener) {
			connection.addEventListener('change', publishSaveData);
			const {removeEventListener} = connection;
			detachConnectionListener = removeEventListener
				? () => removeEventListener.call(connection, 'change', publishSaveData)
				: () => {};
		}
	}
	return () => {
		saveDataListeners.delete(listener);
		if (saveDataListeners.size > 0) return;
		detachConnectionListener?.();
		detachConnectionListener = null;
	};
}

export function useSaveData(): boolean {
	const [saveData, setSaveData] = useState(readSaveData);
	useEffect(() => {
		const unsubscribe = subscribeSaveData(setSaveData);
		cachedSaveData = readSaveData();
		setSaveData(cachedSaveData);
		return unsubscribe;
	}, []);
	return saveData;
}
