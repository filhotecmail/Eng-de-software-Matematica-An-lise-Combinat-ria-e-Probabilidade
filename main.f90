! ===============================================================================
! Autor: Carlos Alberto Dias da S. Filho
! Email: filhotecmail@gmail.com
! Data de Criação: 2025-02-01
! Data de Revisão: 2025-02-01
! Sobre o programa: Este é um programa em Fortran F90 para
!   resolver o enunciado do Advent of Code 2025, dia 2.
! Objetivos: Resolver o problema do Dia 2 usando Fortran F90.
!
! Contexto matemático:
! - Um ID inválido é um número decimal cuja representação é a
!   concatenação de uma sequência de dígitos com ela mesma: S||S.
! - IDs inválidos têm número de dígitos par: 2k.
! - k = metade do comprimento e X o inteiro formado pelos
!   primeiros k dígitos - sem zeros à esquerda . Então:
!     N = X * 10^k + X = X * (10^k + 1).
! - Para um intervalo [L, R], e cada k em {1,2,3,4,5} (até 10 dígitos):
!     B_k = 10^k + 1,
!     X ∈ [ceil(L / B_k), floor(R / B_k)] ∩ [10^(k−1), 10^k − 1].
!   Os candidatos inválidos são N = X * B_k.
! - Essa forma evita varrer número a número, gera apenas N possíveis
!   diretamente via X e k.
! - Para intervalos sobrepostos, N deve ser deduplicado antes da soma.
! =================================================================================

program aoc_day2
  use aoc_day2_mod
  implicit none
  ! Ponto de entrada: delega execução ao procedimento principal
  call run

end program aoc_day2

module aoc_day2_mod
  implicit none
  ! Kind inteiro com ~64 bits para suportar IDs grandes
  integer, parameter :: ik = selected_int_kind(18)
  type interval_t
    ! Limites do intervalo [lo, hi]
    integer(ik) :: lo
    integer(ik) :: hi
  end type interval_t
contains
  subroutine read_input_line(path, line)
    character(len=*), intent(in) :: path
    character(len=*), intent(out) :: line
    integer :: unit, ios
    unit = 10              ! Unidade de arquivo
    line = ''              ! Inicializa linha de saída
    open(unit=unit, file=path, status='old', iostat=ios) ! Abre arquivo existente
    if (ios /= 0) return   ! Falha ao abrir: retorna linha vazia
    read(unit,'(A)') line  ! Lê linha única contendo todos os intervalos
    close(unit)            ! Fecha arquivo
  end subroutine read_input_line

  subroutine parse_line(line, intervals, count)
    character(len=*), intent(in) :: line
    type(interval_t), intent(out) :: intervals(:)
    integer, intent(out) :: count
    character(len=4096) :: s
    character(len=128) :: t, a, b
    integer :: pos, lenl, comma_pos, hypos, n
    integer(ik) :: lo, hi
    s = trim(line)         ! Normaliza espaços finais
    lenl = len_trim(s)
    n = 0
    pos = 1
    do                     ! Varre tokens separados por vírgula
      if (pos > lenl) exit
      comma_pos = index(s(pos:lenl), ',')
      if (comma_pos == 0) then
        t = trim(s(pos:lenl))
        pos = lenl + 1
      else
        t = trim(s(pos:pos+comma_pos-2))
        pos = pos + comma_pos
      end if
      if (len_trim(t) == 0) cycle
      hypos = index(t, '-') ! Divide token em "a-b"
      if (hypos == 0) cycle
      a = trim(t(1:hypos-1))
      b = trim(t(hypos+1:len_trim(t)))
      read(a, *) lo        ! Converte limites
      read(b, *) hi
      if (lo <= hi) then
        if (n < size(intervals)) then
          n = n + 1
          intervals(n)%lo = lo
          intervals(n)%hi = hi
        end if
      end if
    end do
    count = n              ! Retorna quantidade de intervalos parseados
  end subroutine parse_line

  pure function pow10(k) result(p)
    integer, intent(in) :: k
    integer(ik) :: p
    integer :: i
    p = 1_ik
    do i = 1, k
      p = p * 10_ik
    end do
  end function pow10

  pure function ceil_div(a, b) result(c)
    integer(ik), intent(in) :: a, b
    integer(ik) :: c
    if (b <= 0_ik) then
      c = 0_ik
    else
      c = (a + b - 1_ik) / b
    end if
  end function ceil_div

  recursive subroutine quicksort(a, l, r)
    integer(ik), intent(inout) :: a(:)
    integer, intent(in) :: l, r
    integer(ik) :: pivot, tmp
    integer :: i, j, m
    if (l >= r) return
    m = (l + r) / 2
    pivot = a(m)
    i = l
    j = r
    do
      do while (a(i) < pivot)
        i = i + 1
      end do
      do while (a(j) > pivot)
        j = j - 1
      end do
      if (i <= j) then
        tmp = a(i); a(i) = a(j); a(j) = tmp
        i = i + 1
        j = j - 1
      end if
      if (i > j) exit
    end do
    call quicksort(a, l, j)
    call quicksort(a, i, r)
  end subroutine quicksort

  subroutine compute_total(intervals, count, total)
    type(interval_t), intent(in) :: intervals(:)
    integer, intent(in) :: count
    integer(ik), intent(out) :: total
    integer :: k, idx, i
    integer(ik) :: bk, xlo, xhi, lo_k, hi_k
    integer, parameter :: max_vals = 300000
    integer(ik), allocatable :: vals(:)
    integer :: nvals
    allocate(vals(max_vals))
    nvals = 0
    do k = 1, 5
      bk = pow10(k) + 1_ik
      lo_k = pow10(k-1)
      hi_k = pow10(k) - 1_ik
      do idx = 1, count
        xlo = ceil_div(intervals(idx)%lo, bk)
        xhi = intervals(idx)%hi / bk
        if (xlo < lo_k) xlo = lo_k
        if (xhi > hi_k) xhi = hi_k
        if (xlo <= xhi) then
          do i = 0, xhi - xlo
            if (nvals < max_vals) then
              nvals = nvals + 1
              vals(nvals) = (xlo + i) * bk
            end if
          end do
        end if
      end do
    end do
    if (nvals > 1) call quicksort(vals, 1, nvals)
    total = 0_ik
    do i = 1, nvals
      if (i == 1 .or. vals(i) /= vals(i-1)) total = total + vals(i)
    end do
    deallocate(vals)
  end subroutine compute_total

  subroutine run
    character(len=4096) :: input_line
    type(interval_t) :: intervals(1024)
    integer :: count
    integer(ik) :: total
    ! Caminho absoluto para a entrada com uma linha de intervalos
    call read_input_line('c:\projetos de estudos\AdventofCode\202502\Enunciado do problema\Imput de dados par analise.txt', input_line)
    ! Converte a linha em vetor de intervalos [lo,hi]
    call parse_line(input_line, intervals, count)
    call compute_total(intervals, count, total)
    print *, total
  end subroutine run
end module aoc_day2_mod
