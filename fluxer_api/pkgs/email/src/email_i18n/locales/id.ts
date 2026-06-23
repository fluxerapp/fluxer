// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_ID_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Akun {product_name}mu telah dinonaktifkan sementara",
		"body": "Halo {username},\n\nKami menonaktifkan sementara akun {product_name}mu karena kami mendeteksi aktivitas mencurigakan.\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nUntuk mendapatkan kembali akses ke akunmu, kamu perlu mengatur ulang kata sandimu:\n\n{forgotUrl}\n\nSetelah kamu mengatur ulang kata sandimu, kamu akan bisa masuk lagi.\n\nKalau kamu merasa ini salah, hubungi tim dukungan kami.\n\n– Tim Keamanan {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Akun {product_name}mu akan dihapus secara permanen",
		"body": "Halo {username},\n\nAkun {product_name}mu telah dijadwalkan untuk dihapus permanen karena melanggar Ketentuan Layanan atau Pedoman Komunitas kami.\n\nPenghapusan terjadwal: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nIni tindakan serius. Data akunmu akan dihapus permanen pada tanggal yang dijadwalkan.\n\nTinjau:\n- Ketentuan Layanan: {termsUrl}\n- Pedoman Komunitas: {guidelinesUrl}\n\nProses banding:\nJika kamu yakin keputusan ini keliru atau tidak beralasan, kamu punya 60 hari untuk mengajukan banding. Kirim email ke {appeals_email} dari alamat email ini.\n\nDalam bandingmu:\n- Jelaskan dengan jelas kenapa kamu yakin keputusan ini keliru atau tidak beralasan\n- Berikan bukti atau konteks yang relevan\n\nAnggota Tim Keamanan {product_name} akan meninjau bandingmu dan bisa menunda penghapusan sampai keputusan akhir tercapai.\n\n– Tim Keamanan {product_name}"
	},
	"account_temp_banned": {
		"subject": "Akun {product_name}mu telah ditangguhkan sementara",
		"body": "Halo {username},\n\nAkun {product_name}mu telah ditangguhkan sementara karena melanggar Ketentuan Layanan atau Pedoman Komunitas kami.\n\nDurasi: {durationHours, plural,\n  =1 {1 jam}\n  other {# jam}\n}\nDitangguhkan hingga: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nSelama waktu ini, kamu tidak akan bisa mengakses akunmu.\n\nTinjau:\n- Ketentuan Layanan: {termsUrl}\n- Pedoman Komunitas: {guidelinesUrl}\n\nJika kamu yakin keputusan ini keliru atau tidak beralasan, kamu bisa mengajukan banding. Kirim email ke {appeals_email} dari alamat email ini dan jelaskan dengan jelas kenapa kamu yakin keputusan ini salah. Kami akan meninjau bandingmu dan menanggapi dengan keputusan kami.\n\n– Tim Keamanan {product_name}"
	},
	"donation_confirmation": {
		"subject": "Terima kasih atas donasi {product_name}mu",
		"body": "Halo,\n\nTerima kasih atas donasimu ke {product_name}! Donasi {interval, select,\n  month {berulang}\n  year {berulang}\n  other {satu kali}\n}mu telah berhasil {interval, select,\n  month {diatur}\n  year {diatur}\n  other {diproses}\n}.\n\nDetail donasi:\nJumlah: {amount} {currency} {interval, select,\n  month {per bulan}\n  year {per tahun}\n  other {}\n}\n\nStripe akan mengirimkan email tanda terima terpisah dengan PDF fakturmu segera. Ini mencakup semua detail pembayaran dan bisa dipakai untuk keperluan pajak.\n\nKamu bisa lihat riwayat donasimu, unduh faktur, {interval, select,\n  month {dan kelola atau batalkan langgananmu}\n  year {dan kelola atau batalkan langgananmu}\n  other {dan kelola donasi di masa depan}\n} kapan saja menggunakan tautan ini:\n\n{manageUrl}\n\nDonasimu membantu {product_name} tetap jalan. Terima kasih!\n\n– Tim {product_name}"
	},
	"donation_magic_link": {
		"subject": "Kelola donasi {product_name}mu",
		"body": "Halo,\n\nKlik tautan di bawah untuk mengakses portal donaturmu:\n\n{manageUrl}\n\nDi portal, kamu bisa kelola langganan, unduh faktur, dan lihat riwayat donasimu.\n\nTautan ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nKalau kamu tidak meminta tautan ini, kamu bisa mengabaikan email ini.\n\n– Tim {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Verifikasi emailmu untuk laporan DSA",
		"body": "Halo,\n\nGunakan kode verifikasi di bawah ini untuk mengirimkan laporan Digital Services Actmu di {product_name}:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nKalau kamu tidak meminta ini, kamu bisa abaikan email ini.\n\n– Tim Keamanan {product_name}"
	},
	"email_change_new": {
		"subject": "Verifikasi email {product_name} barumu",
		"body": "Halo {username},\n\nMasukkan kode ini di aplikasi untuk memverifikasi email {product_name} barumu:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nKalau kamu tidak meminta ini, kamu bisa abaikan email ini.\n\n– Tim {product_name}"
	},
	"email_change_original": {
		"subject": "Konfirmasi perubahan email {product_name}mu",
		"body": "Halo {username},\n\nKami menerima permintaan untuk mengubah alamat email di akun {product_name}mu.\n\nUntuk mengonfirmasi perubahan ini, masukkan kode ini di aplikasi:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nJika kamu tidak meminta ini, segera amankan akunmu.\n\n– Tim {product_name}"
	},
	"email_change_revert": {
		"subject": "Email {product_name}mu telah diubah",
		"body": "Halo {username},\n\nAlamat email di akun {product_name}mu telah diubah menjadi {newEmail}.\n\nKalau kamu yang melakukan perubahan ini, tidak perlu lakukan apa pun. Jika bukan kamu yang melakukan ini, kamu bisa batalkan perubahan dan amankan akunmu menggunakan tautan ini:\n\n{revertUrl}\n\nIni akan mengembalikan emailmu sebelumnya, mengeluarkan kamu dari semua perangkat, hapus nomor telepon yang tertaut, nonaktifkan MFA, dan mewajibkan kamu untuk atur kata sandi baru.\n\n– Tim Keamanan {product_name}"
	},
	"email_verification": {
		"subject": "Verifikasi alamat email {product_name}mu",
		"body": "Halo {username},\n\nVerifikasi alamat email untuk akun {product_name}mu dengan mengeklik tautan di bawah ini:\n\n{verifyUrl}\n\nKalau kamu tidak membuat akun {product_name}, kamu bisa abaikan email ini dengan aman.\n\nTautan ini berlaku selama 24 jam.\n\n– Tim {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Manfaat dari hadiah yang kamu tukarkan telah dihapus",
		"body": "Halo {username},\n\nKode hadiah yang kamu tukarkan awalnya dibayar oleh orang lain. Pembayaran itu sudah dibatalkan (chargeback).\n\nKarena ini, kami sudah hapus manfaat yang ditambahkan ke akunmu saat kamu menukarkan hadiah itu.\n\nJika kamu merasa ini adalah kesalahan, hubungi tim dukungan kami dan sertakan detail apa pun yang kamu miliki tentang kode hadiah dan kapan kamu menukarkannya.\n\n– Tim {product_name}"
	},
	"harvest_completed": {
		"subject": "Ekspor data {product_name}mu siap diunduh",
		"body": "Halo {username},\n\nEkspor datamu sudah siap.\n\nTautan unduh:\n{downloadUrl}\n\nPesan yang disertakan: {totalMessages, number}\nUkuran file: {fileSizeMB, number} MB\n\nTautan ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nJika kamu tidak meminta ekspor ini, segera ubah kata sandimu dan hubungi tim dukungan kami.\n\n– Tim {product_name}"
	},
	"inactivity_warning": {
		"subject": "Akun {product_name}mu akan dihapus karena tidak aktif",
		"body": "Halo {username},\n\nKami belum melihat aktivitas apa pun di akun {product_name}mu sejak {lastActiveDate, date, full}.\n\nJika kamu tidak masuk sebelum {deletionDate, date, full} pukul {deletionDate, time, short}, akunmu akan dihapus permanen karena tidak aktif.\n\nMasuk di sini:\n{loginUrl}\n\nJika kamu baru saja menggunakan {product_name}, segera hubungi tim dukungan kami.\n\n– Tim {product_name}"
	},
	"ip_authorization": {
		"subject": "Otorisasi masuk dari alamat IP barumu",
		"body": "Halo {username},\n\nKami mendeteksi upaya masuk ke akun {product_name}mu dari alamat IP baru:\n\nAlamat IP: {ipAddress}\nLokasi: {location}\n\nKalau ini kamu, otorisasi alamat IP ini dengan mengeklik tautan di bawah ini:\n\n{authUrl}\n\nJika kamu tidak mencoba masuk, segera ubah kata sandimu.\n\nTautan ini berlaku selama 30 menit.\n\n– Tim {product_name}"
	},
	"password_change_verification": {
		"subject": "Konfirmasi perubahan kata sandi {product_name}mu",
		"body": "Halo {username},\n\nKami menerima permintaan untuk mengubah kata sandi di akun {product_name}mu.\n\nUntuk mengonfirmasi perubahan ini, masukkan kode ini di aplikasi:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt}.\n\nJika kamu tidak meminta ini, seseorang mungkin punya akses ke akunmu. Ubah kata sandimu segera dan aktifkan autentikasi dua faktor.\n\n– Tim {product_name}"
	},
	"password_reset": {
		"subject": "Atur ulang kata sandi {product_name}mu",
		"body": "Halo {username},\n\nKamu meminta pengaturan ulang kata sandi {product_name}. Gunakan tautan di bawah ini untuk atur kata sandi baru:\n\n{resetUrl}\n\nJika kamu tidak meminta ini, kamu bisa abaikan email ini dengan aman.\n\nTautan ini berlaku selama 1 jam.\n\n– Tim {product_name}"
	},
	"registration_approved": {
		"subject": "Pendaftaran {product_name}mu telah disetujui",
		"body": "Halo {username},\n\nKabar baik: pendaftaran {product_name}mu telah disetujui.\n\nKamu sekarang bisa masuk ke aplikasi {product_name} lewat sini:\n{channelsUrl}\n\nSelamat datang di komunitas {product_name}.\n\n– Tim {product_name}"
	},
	"report_resolved": {
		"subject": "Laporan {product_name}mu telah ditinjau",
		"body": "Halo {username},\n\nLaporanmu (ID: {reportId}) telah ditinjau oleh Tim Keamanan kami.{hasComment, select, yes {\n\nTanggapan dari Tim Keamanan:\n{publicComment}} other {}}\n\nTerima kasih telah membantu menjaga {product_name} tetap aman untuk semua orang. Kami menanggapi semua laporan dengan serius dan menghargai kontribusimu ke komunitas.\n\nKalau kamu punya pertanyaan atau kekhawatiran tentang hasil ini, hubungi {safety_email}.\n\n– Tim Keamanan {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Akun {product_name}mu akan dihapus secara permanen",
		"body": "Halo {username},\n\nAkun {product_name}mu telah dijadwalkan untuk dihapus permanen.\n\nPenghapusan terjadwal: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nIni tindakan serius. Data akunmu akan dihapus permanen pada tanggal yang dijadwalkan.\n\nJika kamu yakin keputusan ini keliru, kamu bisa ajukan banding. Kirim email ke {appeals_email} dari alamat email ini.\n\n– Tim Keamanan {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "Penghapusan akun {product_name}mu dijadwalkan",
		"body": "Halo {username},\n\nKamu meminta penghapusan akun {product_name}mu. Akunmu dijadwalkan untuk dihapus permanen pada:\n\n{deletionDate, date, full} pukul {deletionDate, time, short}\n\nJika kamu tidak meminta ini, masuk ke akunmu untuk membatalkan penghapusan. Kami juga menyarankan untuk mengubah kata sandimu untuk mengamankan akunmu.\n\n– Tim {product_name}"
	},
	"unban_notification": {
		"subject": "Penangguhan akun {product_name}mu telah dicabut",
		"body": "Halo {username},\n\nKabar baik: penangguhan akun {product_name}mu telah dicabut.\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nKamu sekarang bisa masuk lagi dan terus menggunakan {product_name} seperti biasa.\n\n– Tim Keamanan {product_name}"
	}
});

export default EMAIL_I18N_ID_MESSAGES;
