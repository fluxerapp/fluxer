// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '@pkgs/email/src/email_i18n/EmailI18nMessages';

const EMAIL_I18N_ID_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Akun {product_name} kamu telah dinonaktifkan sementara",
		"body": "Halo {username},\n\nKami menonaktifkan sementara akun {product_name} kamu karena kami mendeteksi aktivitas mencurigakan.\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nUntuk mendapatkan kembali akses ke akunmu, kamu perlu mengatur ulang kata sandimu:\n\n{forgotUrl}\n\nSetelah mengatur ulang kata sandi, kamu bisa masuk lagi.\n\nJika kamu merasa ini keliru, silakan hubungi tim dukungan kami.\n\n– Tim Keamanan {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Akun {product_name} kamu akan dihapus secara permanen",
		"body": "Halo {username},\n\nAkun {product_name} kamu telah dijadwalkan untuk dihapus permanen karena melanggar Ketentuan Layanan atau Pedoman Komunitas kami.\n\nPenghapusan terjadwal: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nIni adalah tindakan penegakan yang serius. Data akunmu akan dihapus permanen pada tanggal yang dijadwalkan.\n\nSilakan tinjau:\n- Ketentuan Layanan: {termsUrl}\n- Pedoman Komunitas: {guidelinesUrl}\n\nProses banding:\nJika kamu yakin keputusan ini keliru atau tidak beralasan, kamu punya 60 hari untuk mengajukan banding. Kirim email ke {appeals_email} dari alamat email ini.\n\nDalam bandingmu:\n- Uraikan dengan jelas kenapa kamu yakin keputusan ini keliru atau tidak beralasan\n- Berikan bukti atau konteks yang relevan\n\nAnggota Tim Keamanan {product_name} akan meninjau bandingmu dan bisa menunda penghapusan sampai keputusan akhir tercapai.\n\n– Tim Keamanan {product_name}"
	},
	"account_temp_banned": {
		"subject": "Akun {product_name} kamu telah ditangguhkan sementara",
		"body": "Halo {username},\n\nAkun {product_name} kamu telah ditangguhkan sementara karena melanggar Ketentuan Layanan atau Pedoman Komunitas kami.\n\nDurasi: {durationHours, plural,\n  =1 {1 jam}\n  other {# jam}\n}\nDitangguhkan hingga: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nSelama waktu ini, kamu tidak akan bisa mengakses akunmu.\n\nSilakan tinjau:\n- Ketentuan Layanan: {termsUrl}\n- Pedoman Komunitas: {guidelinesUrl}\n\nJika kamu yakin keputusan ini keliru atau tidak beralasan, kamu bisa mengajukan banding. Kirim email ke {appeals_email} dari alamat email ini dan uraikan dengan jelas kenapa kamu yakin keputusan ini keliru. Kami akan meninjau bandingmu dan menyampaikan keputusan kami.\n\n– Tim Keamanan {product_name}"
	},
	"donation_confirmation": {
		"subject": "Terima kasih atas donasimu untuk {product_name}",
		"body": "Halo,\n\nTerima kasih atas donasimu untuk {product_name}! Donasi {interval, select,\n  month {berulang}\n  year {berulang}\n  other {satu kali}\n} kamu telah berhasil {interval, select,\n  month {diatur}\n  year {diatur}\n  other {diproses}\n}.\n\nDetail donasi:\nJumlah: {amount} {currency} {interval, select,\n  month {per bulan}\n  year {per tahun}\n  other {}\n}\n\nStripe akan segera mengirimi kamu email tanda terima terpisah beserta PDF fakturmu. Ini mencakup semua detail pembayaran dan bisa dipakai untuk keperluan pajak.\n\nKamu bisa melihat riwayat donasimu, mengunduh faktur, {interval, select,\n  month {dan mengelola atau membatalkan langgananmu}\n  year {dan mengelola atau membatalkan langgananmu}\n  other {dan mengelola donasi di masa mendatang}\n} kapan saja lewat tautan ini:\n\n{manageUrl}\n\nDukunganmu membantu {product_name} tetap berjalan. Terima kasih!\n\n– Tim {product_name}"
	},
	"donation_magic_link": {
		"subject": "Kelola donasimu untuk {product_name}",
		"body": "Halo,\n\nKlik tautan di bawah untuk mengakses portal donaturmu:\n\n{manageUrl}\n\nDi portal itu, kamu bisa mengelola langganan, mengunduh faktur, dan melihat riwayat donasimu.\n\nTautan ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nJika kamu tidak meminta tautan ini, kamu bisa mengabaikan email ini dengan aman.\n\n– Tim {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Verifikasi emailmu untuk laporan DSA",
		"body": "Halo,\n\nGunakan kode verifikasi di bawah ini untuk mengirimkan laporan Digital Services Act kamu di {product_name}:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nJika kamu tidak meminta ini, kamu bisa mengabaikan email ini.\n\n– Tim Keamanan {product_name}"
	},
	"email_change_new": {
		"subject": "Verifikasi email {product_name} barumu",
		"body": "Halo {username},\n\nMasukkan kode ini di aplikasi untuk memverifikasi email {product_name} barumu:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nJika kamu tidak meminta ini, kamu bisa mengabaikan email ini.\n\n– Tim {product_name}"
	},
	"email_change_original": {
		"subject": "Konfirmasi perubahan email {product_name} kamu",
		"body": "Halo {username},\n\nKami menerima permintaan untuk mengubah alamat email di akun {product_name} kamu.\n\nUntuk mengonfirmasi perubahan ini, masukkan kode ini di aplikasi:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nJika kamu tidak meminta ini, segera amankan akunmu.\n\n– Tim {product_name}"
	},
	"email_change_revert": {
		"subject": "Email {product_name} kamu telah diubah",
		"body": "Halo {username},\n\nAlamat email di akun {product_name} kamu telah diubah menjadi {newEmail}.\n\nJika ini memang kamu, tidak ada yang perlu dilakukan. Jika bukan, kamu bisa membatalkan perubahan itu dan mengamankan akunmu lewat tautan ini:\n\n{revertUrl}\n\nTindakan ini akan mengembalikan email lamamu, mengeluarkanmu dari semua perangkat, menghapus nomor telepon yang tertaut, menonaktifkan MFA, dan mengharuskanmu membuat kata sandi baru.\n\n– Tim Keamanan {product_name}"
	},
	"email_verification": {
		"subject": "Verifikasi alamat email {product_name} kamu",
		"body": "Halo {username},\n\nSilakan verifikasi alamat email untuk akun {product_name} kamu dengan mengeklik tautan di bawah ini:\n\n{verifyUrl}\n\nJika kamu tidak membuat akun {product_name}, kamu bisa mengabaikan email ini dengan aman.\n\nTautan ini berlaku selama 24 jam.\n\n– Tim {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Manfaat dari hadiah yang kamu tukarkan telah dihapus",
		"body": "Halo {username},\n\nKode hadiah yang kamu tukarkan awalnya dibayar oleh orang lain. Pembayaran itu kemudian dibatalkan (chargeback).\n\nKarena itu, kami menghapus manfaat yang ditambahkan ke akunmu saat kamu menukarkan hadiah tersebut.\n\nJika kamu merasa ini keliru, silakan hubungi tim dukungan kami dan sertakan detail apa pun yang kamu punya tentang kode hadiah itu dan kapan kamu menukarkannya.\n\n– Tim {product_name}"
	},
	"harvest_completed": {
		"subject": "Ekspor data {product_name} kamu siap diunduh",
		"body": "Halo {username},\n\nEkspor datamu sudah siap.\n\nTautan unduh:\n{downloadUrl}\n\nPesan yang disertakan: {totalMessages, number}\nUkuran file: {fileSizeMB, number} MB\n\nTautan ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nJika kamu tidak meminta ekspor ini, segera ubah kata sandimu dan hubungi tim dukungan kami.\n\n– Tim {product_name}"
	},
	"inactivity_warning": {
		"subject": "Akun {product_name} kamu akan dihapus karena tidak aktif",
		"body": "Halo {username},\n\nKami belum melihat aktivitas apa pun di akun {product_name} kamu sejak {lastActiveDate, date, full}.\n\nJika kamu tidak masuk sebelum {deletionDate, date, full} pukul {deletionDate, time, short}, akunmu akan dihapus permanen karena tidak aktif.\n\nMasuk di sini:\n{loginUrl}\n\nJika kamu baru saja menggunakan {product_name}, segera hubungi tim dukungan kami.\n\n– Tim {product_name}"
	},
	"ip_authorization": {
		"subject": "Otorisasi masuk dari alamat IP baru",
		"body": "Halo {username},\n\nKami mendeteksi upaya masuk ke akun {product_name} kamu dari alamat IP baru:\n\nAlamat IP: {ipAddress}\nLokasi: {location}\n\nJika ini memang kamu, silakan otorisasi alamat IP ini dengan mengeklik tautan di bawah ini:\n\n{authUrl}\n\nJika kamu tidak mencoba masuk, segera ubah kata sandimu.\n\nTautan ini berlaku selama 30 menit.\n\n– Tim {product_name}"
	},
	"mfa_backup_codes_view": {
		"subject": "Konfirmasi akses ke kode cadangan {product_name} kamu",
		"body": "Halo {username},\n\nKami menerima permintaan untuk melihat kode cadangan di akun {product_name} kamu.\n\nUntuk mengonfirmasi permintaan ini, masukkan kode ini di aplikasi:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt, date, full} pukul {expiresAt, time, short}.\n\nJika kamu tidak meminta ini, seseorang mungkin punya akses ke akunmu. Ubah kata sandimu segera.\n\n– Tim {product_name}"
	},
	"password_change_verification": {
		"subject": "Konfirmasi perubahan kata sandi {product_name} kamu",
		"body": "Halo {username},\n\nKami menerima permintaan untuk mengubah kata sandi di akun {product_name} kamu.\n\nUntuk mengonfirmasi perubahan ini, masukkan kode ini di aplikasi:\n\n{code}\n\nKode ini kedaluwarsa pada {expiresAt}.\n\nJika kamu tidak meminta ini, seseorang mungkin punya akses ke akunmu. Ubah kata sandimu segera dan aktifkan autentikasi dua faktor.\n\n– Tim {product_name}"
	},
	"password_reset": {
		"subject": "Atur ulang kata sandi {product_name} kamu",
		"body": "Halo {username},\n\nKamu meminta pengaturan ulang kata sandi {product_name}. Gunakan tautan di bawah ini untuk mengatur kata sandi baru:\n\n{resetUrl}\n\nJika kamu tidak meminta ini, kamu bisa mengabaikan email ini dengan aman.\n\nTautan ini berlaku selama 1 jam.\n\n– Tim {product_name}"
	},
	"registration_approved": {
		"subject": "Pendaftaranmu di {product_name} telah disetujui",
		"body": "Halo {username},\n\nKabar baik: pendaftaranmu di {product_name} telah disetujui.\n\nKamu sekarang bisa masuk ke aplikasi {product_name} di sini:\n{channelsUrl}\n\nSelamat datang di komunitas {product_name}.\n\n– Tim {product_name}"
	},
	"report_resolved": {
		"subject": "Laporanmu di {product_name} telah ditinjau",
		"body": "Halo {username},\n\nLaporanmu (ID: {reportId}) telah ditinjau oleh Tim Keamanan kami.{hasComment, select, yes {\n\nTanggapan dari Tim Keamanan:\n{publicComment}} other {}}\n\nTerima kasih telah membantu menjaga {product_name} tetap aman untuk semua orang. Kami menanggapi semua laporan dengan serius dan menghargai kontribusimu bagi komunitas.\n\nJika kamu punya pertanyaan atau kekhawatiran tentang hasil ini, silakan hubungi {safety_email}.\n\n– Tim Keamanan {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Akun {product_name} kamu akan dihapus secara permanen",
		"body": "Halo {username},\n\nAkun {product_name} kamu telah dijadwalkan untuk dihapus permanen.\n\nPenghapusan terjadwal: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nIni adalah tindakan penegakan yang serius. Data akunmu akan dihapus permanen pada tanggal yang dijadwalkan.\n\nJika kamu yakin keputusan ini keliru, kamu bisa mengajukan banding. Kirim email ke {appeals_email} dari alamat email ini.\n\n– Tim Keamanan {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "Penghapusan akun {product_name} kamu telah dijadwalkan",
		"body": "Halo {username},\n\nKamu meminta agar akun {product_name} kamu dihapus. Akunmu dijadwalkan untuk dihapus permanen pada:\n\n{deletionDate, date, full} pukul {deletionDate, time, short}\n\nJika kamu tidak meminta ini, masuk ke akunmu untuk membatalkan penghapusan. Kami juga menyarankan kamu mengubah kata sandi agar akunmu tetap aman.\n\n– Tim {product_name}"
	},
	"unban_notification": {
		"subject": "Penangguhan akun {product_name} kamu telah dicabut",
		"body": "Halo {username},\n\nKabar baik: penangguhan akun {product_name} kamu telah dicabut.\n\n{reason, select,\n  null {}\n  other {Alasan: {reason}}\n}\n\nKamu sekarang bisa masuk lagi dan terus menggunakan {product_name} seperti biasa.\n\n– Tim Keamanan {product_name}"
	}
});

export default EMAIL_I18N_ID_MESSAGES;
