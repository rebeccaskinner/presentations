.section .data
.align 8
.align 1
.LrTW_closure:
	.quad	ghczmprim_GHCziTypes_Izh_con_info
	.quad	1
.section .data
.align 8
.align 1
.LrU6_closure:
	.quad	ghczmprim_GHCziTypes_Izh_con_info
	.quad	0
.section .data
.align 8
.align 1
.LuV8_srt:
	.quad	stg_SRT_2_info
	.quad	base_GHCziList_zzipWith_closure
	.quad	base_GHCziNum_zdfNumInt_closure
	.quad	0
.section .data
.align 8
.align 1
.LuV9_srt:
	.quad	stg_SRT_4_info
	.quad	base_GHCziList_tail_closure
	.quad	base_GHCziBase_zdfApplicativezmzg_closure
	.quad	Main_fibs_closure
	.quad	.LsUj_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	base_GHCziNum_zdfNumInt_closure-(.LsUi_info)+0
.LsUi_info:
.LcUR:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcUS
.LcUT:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $base_GHCziNum_zdfNumInt_closure,%r14d
	addq $-16,%rbp
	jmp base_GHCziNum_zp_info
.LcUS:
	jmp *-16(%r13)
	.size .LsUi_info, .-.LsUi_info
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.LuV8_srt-(.LsUj_info)+0
.LsUj_info:
.LcUU:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcUV
.LcUW:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .LcUY
.LcUX:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcUM
.LcUL:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movq $.LsUi_info,-8(%r12)
	leaq -8(%r12),%rax
	movq %rax,%r14
	movl $base_GHCziList_zzipWith_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_p_fast
.LcUM:
	jmp *(%rbx)
.LcUY:
	movq $16,904(%r13)
.LcUV:
	jmp *-16(%r13)
	.size .LsUj_info, .-.LsUj_info
.section .data
.align 8
.align 1
.LsUj_closure:
	.quad	.LsUj_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.LrU7_closure:
	.quad	ghczmprim_GHCziTypes_ZC_con_info
	.quad	.LrTW_closure+1
	.quad	.LrU8_closure
	.quad	0
.section .data
.align 8
.align 1
.globl Main_fibs_closure
.type Main_fibs_closure, @object
Main_fibs_closure:
	.quad	ghczmprim_GHCziTypes_ZC_con_info
	.quad	.LrU6_closure+1
	.quad	.LrU7_closure+2
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.LuV9_srt-(.LrU8_info)+0
.LrU8_info:
.LcV5:
	leaq -48(%rbp),%rax
	cmpq %r15,%rax
	jb .LcV6
.LcV7:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcV4
.LcV3:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $base_GHCziBase_zdfApplicativezmzg_closure,%r14d
	movq $stg_ap_ppp_info,-48(%rbp)
	movq $.LsUj_closure,-40(%rbp)
	movq $base_GHCziList_tail_closure,-32(%rbp)
	movq $Main_fibs_closure+2,-24(%rbp)
	addq $-48,%rbp
	jmp base_GHCziBase_zlztzg_info
.LcV4:
	jmp *(%rbx)
.LcV6:
	jmp *-16(%r13)
	.size .LrU8_info, .-.LrU8_info
.section .data
.align 8
.align 1
.LrU8_closure:
	.quad	.LrU8_info
	.quad	0
	.quad	0
	.quad	0
.section .text
.align 8
.align 8
	.quad	4294967301
	.quad	2
	.long	14
	.long	0
.globl Main_fib_info
.type Main_fib_info, @function
Main_fib_info:
.LcVv:
.LcVx:
	movq %r14,%rsi
	movl $Main_fibs_closure+2,%r14d
	movl $base_GHCziList_znzn_closure,%ebx
	jmp stg_ap_pp_fast
	.size Main_fib_info, .-Main_fib_info
.section .data
.align 8
.align 1
.globl Main_fib_closure
.type Main_fib_closure, @object
Main_fib_closure:
	.quad	Main_fib_info
	.quad	base_GHCziList_znzn_closure
	.quad	Main_fibs_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	1
	.long	16
	.long	Main_fib_closure-(.LsUo_info)+0
.LsUo_info:
.LcVJ:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcVK
.LcVL:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 16(%rbx),%rax
	movq %rax,%rsi
	movl $Main_fibs_closure+2,%r14d
	movl $base_GHCziList_znzn_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcVK:
	jmp *-16(%r13)
	.size .LsUo_info, .-.LsUo_info
.section .text
.align 8
.align 8
	.quad	1
	.long	16
	.long	Main_fib_closure-(.LsUn_info)+0
.LsUn_info:
.LcVQ:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcVR
.LcVS:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 16(%rbx),%rax
	movq %rax,%rsi
	movl $Main_fibs_closure+2,%r14d
	movl $base_GHCziList_znzn_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcVR:
	jmp *-16(%r13)
	.size .LsUn_info, .-.LsUn_info
.section .text
.align 8
.align 8
	.quad	8589934607
	.quad	2
	.long	14
	.long	0
.globl Main_fibSum_info
.type Main_fibSum_info, @function
Main_fibSum_info:
.LcVT:
	leaq -24(%rbp),%rax
	cmpq %r15,%rax
	jb .LcVU
.LcVV:
	addq $48,%r12
	cmpq 856(%r13),%r12
	ja .LcVX
.LcVW:
	movq $.LsUo_info,-40(%r12)
	movq %rsi,-24(%r12)
	leaq -40(%r12),%rax
	movq $.LsUn_info,-16(%r12)
	movq %r14,(%r12)
	leaq -16(%r12),%rbx
	movl $base_GHCziNum_zdfNumInt_closure,%r14d
	movq $stg_ap_pp_info,-24(%rbp)
	movq %rbx,-16(%rbp)
	movq %rax,-8(%rbp)
	addq $-24,%rbp
	jmp base_GHCziNum_zp_info
.LcVX:
	movq $48,904(%r13)
.LcVU:
	movl $Main_fibSum_closure,%ebx
	jmp *-8(%r13)
	.size Main_fibSum_info, .-Main_fibSum_info
.section .data
.align 8
.align 1
.globl Main_fibSum_closure
.type Main_fibSum_closure, @object
Main_fibSum_closure:
	.quad	Main_fibSum_info
	.quad	Main_fib_closure
	.quad	base_GHCziNum_zdfNumInt_closure
	.quad	0
.section .data
.align 8
.align 1
.globl Main_index1_closure
.type Main_index1_closure, @object
Main_index1_closure:
	.quad	ghczmprim_GHCziTypes_Izh_con_info
	.quad	9
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	Main_fib_closure-(Main_fib1_info)+0
.globl Main_fib1_info
.type Main_fib1_info, @function
Main_fib1_info:
.LcWf:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcWg
.LcWh:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcWe
.LcWd:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $Main_index1_closure+1,%esi
	movl $Main_fibs_closure+2,%r14d
	movl $base_GHCziList_znzn_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcWe:
	jmp *(%rbx)
.LcWg:
	jmp *-16(%r13)
	.size Main_fib1_info, .-Main_fib1_info
.section .data
.align 8
.align 1
.globl Main_fib1_closure
.type Main_fib1_closure, @object
Main_fib1_closure:
	.quad	Main_fib1_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.globl Main_index2_closure
.type Main_index2_closure, @object
Main_index2_closure:
	.quad	ghczmprim_GHCziTypes_Izh_con_info
	.quad	11
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	Main_fib_closure-(Main_fib2_info)+0
.globl Main_fib2_info
.type Main_fib2_info, @function
Main_fib2_info:
.LcWu:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcWv
.LcWw:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcWt
.LcWs:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $Main_index2_closure+1,%esi
	movl $Main_fibs_closure+2,%r14d
	movl $base_GHCziList_znzn_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcWt:
	jmp *(%rbx)
.LcWv:
	jmp *-16(%r13)
	.size Main_fib2_info, .-Main_fib2_info
.section .data
.align 8
.align 1
.globl Main_fib2_closure
.type Main_fib2_closure, @object
Main_fib2_closure:
	.quad	Main_fib2_info
	.quad	0
	.quad	0
	.quad	0
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.LrU9_bytes:
	.string "main"
.section .data
.align 8
.align 1
.LrUa_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	.LrU9_bytes
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
.LrUb_bytes:
	.string "Main"
.section .data
.align 8
.align 1
.LrUc_closure:
	.quad	ghczmprim_GHCziTypes_TrNameS_con_info
	.quad	.LrUb_bytes
.section .data
.align 8
.align 1
.globl Main_zdtrModule_closure
.type Main_zdtrModule_closure, @object
Main_zdtrModule_closure:
	.quad	ghczmprim_GHCziTypes_Module_con_info
	.quad	.LrUa_closure+1
	.quad	.LrUc_closure+1
	.quad	3
.section .data
.align 8
.align 1
.LuWQ_srt:
	.quad	stg_SRT_2_info
	.quad	base_TextziPrintf_zdfIsCharChar_closure
	.quad	base_TextziPrintf_zdfPrintfTypeZMZN_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.LuWQ_srt-(.LrUd_info)+0
.LrUd_info:
.LcWN:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcWO
.LcWP:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcWM
.LcWL:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $base_TextziPrintf_zdfIsCharChar_closure,%r14d
	movl $base_TextziPrintf_zdfPrintfTypeZMZN_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_p_fast
.LcWM:
	jmp *(%rbx)
.LcWO:
	jmp *-16(%r13)
	.size .LrUd_info, .-.LrUd_info
.section .data
.align 8
.align 1
.LrUd_closure:
	.quad	.LrUd_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.LuX6_srt:
	.quad	stg_SRT_3_info
	.quad	base_TextziPrintf_zdfPrintfArgInt_closure
	.quad	base_TextziPrintf_zdfPrintfTypezmzg_closure
	.quad	.LrUd_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.LuX6_srt-(.LrUe_info)+0
.LrUe_info:
.LcX3:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcX4
.LcX5:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcX2
.LcX1:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $.LrUd_closure,%esi
	movl $base_TextziPrintf_zdfPrintfArgInt_closure,%r14d
	movl $base_TextziPrintf_zdfPrintfTypezmzg_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcX2:
	jmp *(%rbx)
.LcX4:
	jmp *-16(%r13)
	.size .LrUe_info, .-.LrUe_info
.section .data
.align 8
.align 1
.LrUe_closure:
	.quad	.LrUe_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.LuXm_srt:
	.quad	stg_SRT_3_info
	.quad	base_TextziPrintf_zdfPrintfArgInt_closure
	.quad	base_TextziPrintf_zdfPrintfTypezmzg_closure
	.quad	.LrUe_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.LuXm_srt-(.LrUf_info)+0
.LrUf_info:
.LcXj:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcXk
.LcXl:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcXi
.LcXh:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $.LrUe_closure,%esi
	movl $base_TextziPrintf_zdfPrintfArgInt_closure,%r14d
	movl $base_TextziPrintf_zdfPrintfTypezmzg_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcXi:
	jmp *(%rbx)
.LcXk:
	jmp *-16(%r13)
	.size .LrUf_info, .-.LrUf_info
.section .data
.align 8
.align 1
.LrUf_closure:
	.quad	.LrUf_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.LuXC_srt:
	.quad	stg_SRT_3_info
	.quad	base_TextziPrintf_zdfPrintfArgInt_closure
	.quad	base_TextziPrintf_zdfPrintfTypezmzg_closure
	.quad	.LrUf_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.LuXC_srt-(.LrUg_info)+0
.LrUg_info:
.LcXz:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcXA
.LcXB:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcXy
.LcXx:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $.LrUf_closure,%esi
	movl $base_TextziPrintf_zdfPrintfArgInt_closure,%r14d
	movl $base_TextziPrintf_zdfPrintfTypezmzg_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcXy:
	jmp *(%rbx)
.LcXA:
	jmp *-16(%r13)
	.size .LrUg_info, .-.LrUg_info
.section .data
.align 8
.align 1
.LrUg_closure:
	.quad	.LrUg_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.LuZx_srt:
	.quad	stg_SRT_4_info
	.quad	ghczmprim_GHCziCString_unpackCStringzh_closure
	.quad	base_TextziPrintf_printf_closure
	.quad	Main_fib2_closure
	.quad	.LrUf_closure
	.quad	0
.section .data
.align 8
.align 1
.LuZy_srt:
	.quad	stg_SRT_3_info
	.quad	base_GHCziBase_zd_closure
	.quad	base_SystemziIO_putStrLn_closure
	.quad	.LuZx_srt
	.quad	0
.section .data
.align 8
.align 1
.LuZz_srt:
	.quad	stg_SRT_3_info
	.quad	Main_fib1_closure
	.quad	Main_fib2_closure
	.quad	base_GHCziNum_zdfNumInt_closure
	.quad	0
.section .data
.align 8
.align 1
.LuZA_srt:
	.quad	stg_SRT_4_info
	.quad	ghczmprim_GHCziCString_unpackCStringzh_closure
	.quad	base_TextziPrintf_printf_closure
	.quad	.LrUg_closure
	.quad	.LuZz_srt
	.quad	0
.section .data
.align 8
.align 1
.LuZB_srt:
	.quad	stg_SRT_3_info
	.quad	base_GHCziBase_zd_closure
	.quad	base_SystemziIO_putStrLn_closure
	.quad	.LuZA_srt
	.quad	0
.section .data
.align 8
.align 1
.LuZC_srt:
	.quad	stg_SRT_6_info
	.quad	ghczmprim_GHCziCString_unpackCStringzh_closure
	.quad	base_TextziPrintf_printf_closure
	.quad	Main_fibSum_closure
	.quad	Main_fib1_closure
	.quad	Main_fib2_closure
	.quad	.LrUg_closure
	.quad	0
.section .data
.align 8
.align 1
.LuZD_srt:
	.quad	stg_SRT_3_info
	.quad	base_GHCziBase_zd_closure
	.quad	base_SystemziIO_putStrLn_closure
	.quad	.LuZC_srt
	.quad	0
.section .data
.align 8
.align 1
.LuZE_srt:
	.quad	stg_SRT_3_info
	.quad	base_GHCziBase_zdfMonadIO_closure
	.quad	.LuZB_srt
	.quad	.LuZD_srt
	.quad	0
.section .data
.align 8
.align 1
.LuZF_srt:
	.quad	stg_SRT_2_info
	.quad	.LuZy_srt
	.quad	.LuZE_srt
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	Main_fibSum_closure-(.LsUA_info)+0
.LsUA_info:
.LcY5:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcY6
.LcY7:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $Main_index2_closure+1,%esi
	movl $Main_index1_closure+1,%r14d
	addq $-16,%rbp
	jmp Main_fibSum_info
.LcY6:
	jmp *-16(%r13)
	.size .LsUA_info, .-.LsUA_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
cYc_str:
	.string "Lib calculated: %d + %d = %d"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.LsUz_info)+0
.LsUz_info:
.LcYd:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcYe
.LcYf:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $cYc_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.LcYe:
	jmp *-16(%r13)
	.size .LsUz_info, .-.LsUz_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.LuZC_srt-(.LsUB_info)+0
.LsUB_info:
.LcYg:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcYh
.LcYi:
	addq $32,%r12
	cmpq 856(%r13),%r12
	ja .LcYk
.LcYj:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.LsUA_info,-24(%r12)
	leaq -24(%r12),%rax
	movq $.LsUz_info,-8(%r12)
	leaq -8(%r12),%rbx
	movq %rax,%r9
	movl $Main_fib2_closure,%r8d
	movl $Main_fib1_closure,%edi
	movq %rbx,%rsi
	movl $.LrUg_closure,%r14d
	movl $base_TextziPrintf_printf_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_ppppp_fast
.LcYk:
	movq $32,904(%r13)
.LcYh:
	jmp *-16(%r13)
	.size .LsUB_info, .-.LsUB_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.LuZD_srt-(.LsUC_info)+0
.LsUC_info:
.LcYl:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcYm
.LcYn:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .LcYp
.LcYo:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.LsUB_info,-8(%r12)
	leaq -8(%r12),%rax
	movq %rax,%rsi
	movl $base_SystemziIO_putStrLn_closure,%r14d
	movl $base_GHCziBase_zd_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcYp:
	movq $16,904(%r13)
.LcYm:
	jmp *-16(%r13)
	.size .LsUC_info, .-.LsUC_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.LuZz_srt-(.LsUw_info)+0
.LsUw_info:
.LcYC:
	leaq -40(%rbp),%rax
	cmpq %r15,%rax
	jb .LcYD
.LcYE:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $base_GHCziNum_zdfNumInt_closure,%r14d
	movq $stg_ap_pp_info,-40(%rbp)
	movq $Main_fib1_closure,-32(%rbp)
	movq $Main_fib2_closure,-24(%rbp)
	addq $-40,%rbp
	jmp base_GHCziNum_zp_info
.LcYD:
	jmp *-16(%r13)
	.size .LsUw_info, .-.LsUw_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
cYJ_str:
	.string "We calculated:  %d + %d = %d"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.LsUv_info)+0
.LsUv_info:
.LcYK:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcYL
.LcYM:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $cYJ_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.LcYL:
	jmp *-16(%r13)
	.size .LsUv_info, .-.LsUv_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.LuZA_srt-(.LsUx_info)+0
.LsUx_info:
.LcYN:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcYO
.LcYP:
	addq $32,%r12
	cmpq 856(%r13),%r12
	ja .LcYR
.LcYQ:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.LsUw_info,-24(%r12)
	leaq -24(%r12),%rax
	movq $.LsUv_info,-8(%r12)
	leaq -8(%r12),%rbx
	movq %rax,%r9
	movl $Main_fib2_closure,%r8d
	movl $Main_fib1_closure,%edi
	movq %rbx,%rsi
	movl $.LrUg_closure,%r14d
	movl $base_TextziPrintf_printf_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_ppppp_fast
.LcYR:
	movq $32,904(%r13)
.LcYO:
	jmp *-16(%r13)
	.size .LsUx_info, .-.LsUx_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.LuZB_srt-(.LsUy_info)+0
.LsUy_info:
.LcYS:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcYT
.LcYU:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .LcYW
.LcYV:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.LsUx_info,-8(%r12)
	leaq -8(%r12),%rax
	movq %rax,%rsi
	movl $base_SystemziIO_putStrLn_closure,%r14d
	movl $base_GHCziBase_zd_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcYW:
	movq $16,904(%r13)
.LcYT:
	jmp *-16(%r13)
	.size .LsUy_info, .-.LsUy_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.LuZE_srt-(.LsUD_info)+0
.LsUD_info:
.LcYX:
	leaq -40(%rbp),%rax
	cmpq %r15,%rax
	jb .LcYY
.LcYZ:
	addq $32,%r12
	cmpq 856(%r13),%r12
	ja .LcZ1
.LcZ0:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.LsUC_info,-24(%r12)
	leaq -24(%r12),%rax
	movq $.LsUy_info,-8(%r12)
	leaq -8(%r12),%rbx
	movl $base_GHCziBase_zdfMonadIO_closure,%r14d
	movq $stg_ap_pp_info,-40(%rbp)
	movq %rbx,-32(%rbp)
	movq %rax,-24(%rbp)
	addq $-40,%rbp
	jmp base_GHCziBase_zgzg_info
.LcZ1:
	movq $32,904(%r13)
.LcYY:
	jmp *-16(%r13)
	.size .LsUD_info, .-.LsUD_info
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
cZe_str:
	.string "fib(%d) = %d"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.LsUs_info)+0
.LsUs_info:
.LcZf:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcZg
.LcZh:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $cZe_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.LcZg:
	jmp *-16(%r13)
	.size .LsUs_info, .-.LsUs_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.LuZx_srt-(.LsUt_info)+0
.LsUt_info:
.LcZi:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcZj
.LcZk:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .LcZm
.LcZl:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.LsUs_info,-8(%r12)
	leaq -8(%r12),%rax
	movl $Main_fib2_closure,%r8d
	movl $Main_index2_closure+1,%edi
	movq %rax,%rsi
	movl $.LrUf_closure,%r14d
	movl $base_TextziPrintf_printf_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pppp_fast
.LcZm:
	movq $16,904(%r13)
.LcZj:
	jmp *-16(%r13)
	.size .LsUt_info, .-.LsUt_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.LuZy_srt-(.LsUu_info)+0
.LsUu_info:
.LcZn:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcZo
.LcZp:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .LcZr
.LcZq:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.LsUt_info,-8(%r12)
	leaq -8(%r12),%rax
	movq %rax,%rsi
	movl $base_SystemziIO_putStrLn_closure,%r14d
	movl $base_GHCziBase_zd_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.LcZr:
	movq $16,904(%r13)
.LcZo:
	jmp *-16(%r13)
	.size .LsUu_info, .-.LsUu_info
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.LuZF_srt-(.LsUE_info)+0
.LsUE_info:
.LcZs:
	leaq -40(%rbp),%rax
	cmpq %r15,%rax
	jb .LcZt
.LcZu:
	addq $32,%r12
	cmpq 856(%r13),%r12
	ja .LcZw
.LcZv:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcXO
.LcXN:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movq $.LsUD_info,-24(%r12)
	leaq -24(%r12),%rax
	movq $.LsUu_info,-8(%r12)
	leaq -8(%r12),%rbx
	movl $base_GHCziBase_zdfMonadIO_closure,%r14d
	movq $stg_ap_pp_info,-40(%rbp)
	movq %rbx,-32(%rbp)
	movq %rax,-24(%rbp)
	addq $-40,%rbp
	jmp base_GHCziBase_zgzg_info
.LcXO:
	jmp *(%rbx)
.LcZw:
	movq $32,904(%r13)
.LcZt:
	jmp *-16(%r13)
	.size .LsUE_info, .-.LsUE_info
.section .data
.align 8
.align 1
.LsUE_closure:
	.quad	.LsUE_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.Lu10Z_srt:
	.quad	stg_SRT_4_info
	.quad	ghczmprim_GHCziCString_unpackCStringzh_closure
	.quad	base_TextziPrintf_printf_closure
	.quad	Main_fib1_closure
	.quad	.LrUf_closure
	.quad	0
.section .data
.align 8
.align 1
.Lu110_srt:
	.quad	stg_SRT_3_info
	.quad	base_GHCziBase_zd_closure
	.quad	base_SystemziIO_putStrLn_closure
	.quad	.Lu10Z_srt
	.quad	0
.section .rodata.str,"aMS",@progbits,1
.align 1
.align 1
c10L_str:
	.string "fib(%d) = %d"
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	ghczmprim_GHCziCString_unpackCStringzh_closure-(.LsUp_info)+0
.LsUp_info:
.Lc10M:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc10N
.Lc10O:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movl $c10L_str,%r14d
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_n_fast
.Lc10N:
	jmp *-16(%r13)
	.size .LsUp_info, .-.LsUp_info
.section .text
.align 8
.align 8
	.quad	0
	.long	15
	.long	.Lu10Z_srt-(.LsUq_info)+0
.LsUq_info:
.Lc10P:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc10Q
.Lc10R:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc10T
.Lc10S:
	movq $stg_upd_frame_info,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq $.LsUp_info,-8(%r12)
	leaq -8(%r12),%rax
	movl $Main_fib1_closure,%r8d
	movl $Main_index1_closure+1,%edi
	movq %rax,%rsi
	movl $.LrUf_closure,%r14d
	movl $base_TextziPrintf_printf_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pppp_fast
.Lc10T:
	movq $16,904(%r13)
.Lc10Q:
	jmp *-16(%r13)
	.size .LsUq_info, .-.LsUq_info
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu110_srt-(.LsUr_info)+0
.LsUr_info:
.Lc10U:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc10V
.Lc10W:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja .Lc10Y
.Lc10X:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc10C
.Lc10B:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movq $.LsUq_info,-8(%r12)
	leaq -8(%r12),%rax
	movq %rax,%rsi
	movl $base_SystemziIO_putStrLn_closure,%r14d
	movl $base_GHCziBase_zd_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_pp_fast
.Lc10C:
	jmp *(%rbx)
.Lc10Y:
	movq $16,904(%r13)
.Lc10V:
	jmp *-16(%r13)
	.size .LsUr_info, .-.LsUr_info
.section .data
.align 8
.align 1
.LsUr_closure:
	.quad	.LsUr_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.Lu11o_srt:
	.quad	stg_SRT_3_info
	.quad	base_GHCziBase_zdfMonadIO_closure
	.quad	.LsUr_closure
	.quad	.LsUE_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu11o_srt-(Main_main_info)+0
.globl Main_main_info
.type Main_main_info, @function
Main_main_info:
.Lc11l:
	leaq -40(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc11m
.Lc11n:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc11k
.Lc11j:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $base_GHCziBase_zdfMonadIO_closure,%r14d
	movq $stg_ap_pp_info,-40(%rbp)
	movq $.LsUr_closure,-32(%rbp)
	movq $.LsUE_closure,-24(%rbp)
	addq $-40,%rbp
	jmp base_GHCziBase_zgzg_info
.Lc11k:
	jmp *(%rbx)
.Lc11m:
	jmp *-16(%r13)
	.size Main_main_info, .-Main_main_info
.section .data
.align 8
.align 1
.globl Main_main_closure
.type Main_main_closure, @object
Main_main_closure:
	.quad	Main_main_info
	.quad	0
	.quad	0
	.quad	0
.section .data
.align 8
.align 1
.Lu11E_srt:
	.quad	stg_SRT_2_info
	.quad	base_GHCziTopHandler_runMainIO_closure
	.quad	Main_main_closure
	.quad	0
.section .text
.align 8
.align 8
	.quad	0
	.long	21
	.long	.Lu11E_srt-(ZCMain_main_info)+0
.globl ZCMain_main_info
.type ZCMain_main_info, @function
ZCMain_main_info:
.Lc11B:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lc11C
.Lc11D:
	subq $8,%rsp
	movq %r13,%rax
	movq %rbx,%rsi
	movq %rax,%rdi
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lc11A
.Lc11z:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $Main_main_closure,%r14d
	movl $base_GHCziTopHandler_runMainIO_closure,%ebx
	addq $-16,%rbp
	jmp stg_ap_p_fast
.Lc11A:
	jmp *(%rbx)
.Lc11C:
	jmp *-16(%r13)
	.size ZCMain_main_info, .-ZCMain_main_info
.section .data
.align 8
.align 1
.globl ZCMain_main_closure
.type ZCMain_main_closure, @object
ZCMain_main_closure:
	.quad	ZCMain_main_info
	.quad	0
	.quad	0
	.quad	0
.section .note.GNU-stack,"",@progbits
.ident "GHC 8.10.4"


